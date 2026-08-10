library(readxl)
library(readr)
library(gdxrrw)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(dplyr)
library(ggrepel)

map_data_agg <- read.table("../../data/Analysis_lu/region17_agg.map", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
map_data_r17 <- read.table("../../data/Analysis_lu/region_iso_17.map", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

df_set_agr_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_a.gdx", "IAMC_templatePlus")
df_set_agr_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_b.gdx", "IAMC_templatePlus")
df_set_agr <- full_join(df_set_agr_a, df_set_agr_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_agr <- df_set_agr %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_agr <- df_set_agr %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin"))

df_set_lu_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_a.gdx", "IAMC_templatePlus")
df_set_lu_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_b.gdx", "IAMC_templatePlus")
df_set_lu <- full_join(df_set_lu_a, df_set_lu_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_lu <- df_set_lu %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_lu <- df_set_lu %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))

df_set <- full_join(df_set_agr, df_set_lu, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))

df_set <- df_set %>%
  #filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
  #                   "Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))%>%
  mutate(VEMF=recode(VEMF,Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre="生産性が高い家畜種への変更",Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee="濃厚飼料の投与",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad="ラグーンカバー",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And="嫌気性消化(大規模プラント)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl="嫌気性消化(小規模)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc="糞尿堆肥化",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams="窒素肥料の硫酸アンモニウムへの変更",Emi_Red_equ_CH4_AFO_Agr_Ric_Wat="水田への間断灌漑の導入",Emi_Red_equ_CH4_AFO_Agr_Ric_Res="稲わらのすき込み",
                     Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til="不耕起移植栽培",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer="肥料の高効率利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res="化学肥料の有機肥料による置き換え",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo="被覆肥料の利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin="硝化抑制剤を含む肥料の利用",
                     Emi_Red_CO2_AFO_Lan_Pla="植林",Emi_Red_CO2_AFO_Lan_Ref="再植林",Emi_Red_CO2_AFO_Lan_FPr="森林保護",Emi_Red_CO2_AFO_Lan_Pff="森林火災の防止","Emi_Red_CO2_AFO_Lan_Ril"="環境負荷の少ない伐採",Emi_Red_CO2_AFO_Lan_Enr="自然再生の強化",Emi_Red_CO2_AFO_Lan_Agf="アグロフォレストリー",Emi_Red_CO2_AFO_Lan_Bch="バイオ炭",Emi_Red_CO2_AFO_Lan_Scs="土壌炭素隔離"))
df_set <- df_set %>%
  mutate(IAMC_templatePlus = ifelse(IAMC_templatePlus < 0, -IAMC_templatePlus, IAMC_templatePlus))

df_set_R178 <- filter(df_set,!(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World")))

df_set_R178 <- left_join(map_data_r17, df_set_R178, by = c("X.ISO" = "REMF"))
df_set_R178 <- subset(df_set_R178, select = -X)
df_set_R178 <- subset(df_set_R178, select = -X.asterisks.for.map)
df_set_R178 <- df_set_R178 %>%
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
  ))

df_red_R178 <- df_set_R178 %>%
  mutate(sub = case_when(
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
    VEMF == "硝化抑制剤を含む肥料の利用" ~ "土壌対策"
  ))

df_red_R178_sub <- df_red_R178 %>%
  group_by(X.ISO,YEMF,sub,agg) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

#OECD----------------------------------------------------------------------------------------------------------------------------------
df_OECD <- filter(df_red_R178_sub,agg %in% c("R5OECD90+EU"))
df_OECD <- df_OECD %>% 
  filter(!is.na(IAMC_templatePlus))

df_OECD_sum <- df_OECD %>%
  group_by(X.ISO,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_OECD_2100 <- df_OECD_sum %>%
  filter(YEMF %in% c("2100"))
  
color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8",
  "LULUCF(CDR)" = "#A5D6A7",
  "LULUCF(その他)" = "#90CAF9"
)

g1 <- df_OECD %>%
  mutate(X.ISO = factor(X.ISO, levels = c( "USA","CAN","AUS","TUR","FRA","DEU","ESP","NZL","GBR","POL",
                                           "ITA","JPN","IRL","NLD","GRC","DNK","SWE","ROU","BEL","HUN",
                                           "CZE","PRT","AUT","FIN","LTU","NOR","SVK","CHE","LVA","BGR",
                                           "EST","SVN","ISL","MNE","SRB"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~X.ISO, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=24.5),
        strip.text.x = element_text(size = 17.5),strip.text.y = element_text(size = 17.5),legend.text = element_text(size =24.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g1)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_OECD_sub.png", plot=g1, width = 16.75, height = 16.00)

#Asia----------------------------------------------------------------------------------------------------------------------------------
df_Asia <- filter(df_red_R178_sub,agg %in% c("R5ASIA"))
df_Asia <- df_Asia %>% 
  filter(!is.na(IAMC_templatePlus))

df_Asia_sum <- df_Asia %>%
  group_by(X.ISO,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_Asia_2100 <- df_Asia_sum %>%
  filter(YEMF %in% c("2100"))

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8",
  "LULUCF(CDR)" = "#A5D6A7",
  "LULUCF(その他)" = "#90CAF9"
)

g2 <- df_Asia %>%
  mutate(X.ISO = factor(X.ISO, levels = c("IND","CHN","PAK","BGD","IDN","THA","MMR","VNM","NPL",
                                          "AFG","PHL","MNG","KHM","KOR","MYS","LKA","LAO","PNG","BTN"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~X.ISO, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 17.5),strip.text.y = element_text(size = 17.5),legend.text = element_text(size =22.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g2)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_Asia_sub.png", plot=g2, width = 16.75, height = 8.75)

#Africa_CA----------------------------------------------------------------------------------------------------------------------------------
df_Africa_CA <- filter(df_red_R178_sub,agg %in% c("R5MAF"))
df_Africa_CA <- df_Africa_CA %>% 
  filter(!is.na(IAMC_templatePlus))

df_Africa_CA_sum <- df_Africa_CA %>%
  group_by(X.ISO,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_Africa_CA_sum <- df_Africa_CA_sum %>%
  filter(YEMF %in% c("2100"))

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8",
  "LULUCF(CDR)" = "#A5D6A7",
  "LULUCF(その他)" = "#90CAF9"
)

g3 <- df_Africa_CA %>%
  mutate(X.ISO = factor(X.ISO, levels = c("NGA","ETH","TZA","KEN","ZAF","IRN","TCD","MLI","SOM","UGA",
                                          "MDG","BFA","NER","COD","DZA","CMR","EGY","ZWE","GIN","SEN",
                                          "CAF","CIV","MAR","AGO","MRT","NAM","SAU","ERI","MOZ","GHA",
                                          "SYR","IRQ","BEN","LBY","ZMB","BWA","MWI","RWA","YEM","TUN",
                                          "COG","SLE","TGO","LSO","SWZ","GNB","JOR","BDI","ISR","DJI",
                                          "ARE","GMB","LBN","KWT","OMN","LBR","PSE","GAB","MUS","COM",
                                          "CPV","QAT","BHR","GNQ","STP"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~X.ISO, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=26.5),
        strip.text.x = element_text(size = 17.5),strip.text.y = element_text(size = 17.5),legend.text = element_text(size =26.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g3)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_Africa_CA_sub.png", plot=g3, width = 17.75, height = 20.00)

#Latin----------------------------------------------------------------------------------------------------------------------------------
df_Latin <- filter(df_red_R178_sub,agg %in% c("R5LAM"))
df_Latin <- df_Latin %>% 
  filter(!is.na(IAMC_templatePlus))

df_Latin_sum <- df_Latin %>%
  group_by(X.ISO,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_Latin_2100 <- df_Latin_sum %>%
  filter(YEMF %in% c("2100"))

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8",
  "LULUCF(CDR)" = "#A5D6A7",
  "LULUCF(その他)" = "#90CAF9"
)

g4 <- df_Latin %>%
  mutate(X.ISO = factor(X.ISO, levels = c("BRA","ARG","MEX","COL","VEN","PER","BOL","URY","ECU","PRY",
                                          "CUB","NIC","GTM","DOM","HND","PAN","CRI","GUY","TCA","SUR"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~X.ISO, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 17.5),strip.text.y = element_text(size = 17.5),legend.text = element_text(size =22.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g4)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_Latin_sub.png", plot=g4, width = 16.75, height = 8.75)

#CIS----------------------------------------------------------------------------------------------------------------------------------
df_CIS <- filter(df_red_R178_sub,agg %in% c("R5REF"))
df_CIS <- df_CIS %>% 
  filter(!is.na(IAMC_templatePlus))

df_CIS_sum <- df_CIS %>%
  group_by(X.ISO,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_CIS_2100 <- df_CIS_sum %>%
  filter(YEMF %in% c("2100"))

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8",
  "LULUCF(CDR)" = "#A5D6A7",
  "LULUCF(その他)" = "#90CAF9"
)

g5 <- df_CIS %>%
  mutate(X.ISO = factor(X.ISO, levels = c("RUS","KAZ","UKR","UZB","BLR","TKM","AZE","TJK","KGZ","GEO",
                                          "MDA","ARM"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~X.ISO, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=21.5),
        strip.text.x = element_text(size = 17.5),strip.text.y = element_text(size = 17.5),legend.text = element_text(size =21.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g5)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_CIS_sub.png", plot=g5, width = 16.75, height = 6.75)
