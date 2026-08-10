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

#上位20か国----------------------------------------------------------------------------------------------------------------------------------
df_red_cate <- df_set %>%
  mutate(cate = case_when(
    VEMF == "植林" ~ "LULUCF(CDR)",
    VEMF == "再植林" ~ "LULUCF(CDR)",
    VEMF == "森林保護" ~ "LULUCF(その他)",
    VEMF == "環境負荷の少ない伐採" ~ "LULUCF(その他)",
    VEMF == "自然再生の強化" ~ "LULUCF(その他)",
    VEMF == "森林火災の防止" ~ "LULUCF(その他)",
    VEMF == "アグロフォレストリー" ~ "LULUCF(CDR)",
    VEMF == "バイオ炭" ~ "LULUCF(CDR)",
    VEMF == "土壌炭素隔離" ~ "LULUCF(CDR)",
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

df_red_R20 <- df_red_cate %>%
  filter(REMF %in% c("IND","USA","BRA","CHN","PAK","CAN","RUS","NGA","AUS","ETH",
                     "ARG","BGD","TZA","IDN","KEN","MEX","ZAF","IRN","TCD","MLI"))%>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  ) %>% 
  filter(YEMF %in% c("2100")) %>% 
  mutate(REMF = recode(REMF,
                       IND="India", USA="United States", BRA="Brazil", CHN="China",
                       PAK="Pakistan", CAN="Canada", RUS="Russia", NGA="Nigeria",
                       AUS="Australia", ETH="Ethiopia", ARG="Argentina", BGD="Bangladesh",
                       TZA="Tanzania", IDN="Indonesia", KEN="Kenya", MEX="Mexico",
                       ZAF="South Africa", IRN="Iran", TCD="Chad", MLI="Mali"
  ))

world_data_pop <- world_data_2100[!is.na(world_data_2100$pop_est), ]
world_data_pop<- world_data_pop %>% filter(!is.na(Value))
world_data_pop <- world_data_pop %>%
  group_by(iso_a3_eh) %>%
  summarize(Value = sum((Value)*1000000)/sum((pop_est)))%>%
  filter(iso_a3_eh %in% c("IND","USA","BRA","CHN","PAK","CAN","RUS","NGA","AUS","ETH",
                          "ARG","BGD","TZA","IDN","KEN","MEX","ZAF","IRN","TCD","MLI"))

color_bar <- c("LULUCF(CDR)"="#A5D6A7","LULUCF(その他)"="#90CAF9","農業"="#F4B400")

g1 <- df_red_R20 %>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  mutate(REMF = factor(REMF, levels = c("Mali","Chad","Iran","South Africa","Mexico","Kenya","Indonesia","Tanzania","Bangladesh","Argentina",
                                        "Ethiopia","Australia","Nigeria","Russia","Canada","Pakistan","China","Brazil","United States","India"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=IAMC_templatePlus,fill=cate),stat = "identity",width = 0.8,position = "stack")+
  coord_flip() +
  labs(title = "", x="", y = expression(paste('Mitigation potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_text(size = 16.25,color="black",hjust = 0.5,margin = margin(r = 17)), 
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        strip.text.x = element_text(size = 25),legend.text = element_text(size =15.50),
        legend.position = c(0.80, 0.150),legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),axis.ticks.y = element_blank() ,
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))
#guides(fill= guide_legend(nrow = 2))
plot(g1)

g2 <- world_data_pop %>%
  #mutate(VEMF = factor(VEMF, levels = c("Plantation", "Reforestation", "Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration", "Agroforestry", "Biochar", "Soil carbon sequestration"))) %>%
  mutate(iso_a3_eh = factor(iso_a3_eh, levels = c("MLI","TCD","IRN","ZAF","MEX","KEN","IDN","TZA","BGD","ARG",
                                                  "ETH","AUS","NGA","RUS","CAN","PAK","CHN","BRA","USA","IND"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot(aes(y = iso_a3_eh, x = Value, fill = iso_a3_eh)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8,fill="grey50") +
  #coord_flip() +
  labs(title = "", x=expression(paste('Mitigation intensity (t',{CO[2]},'eq/cap/yr)',seq="")), y = "")+
  theme_bw()+
  #scale_fill_manual(values = color_bar)+
  scale_x_continuous(labels = function(x) scales::comma(abs(x)))+
  #scale_x_reverse()+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size = 17),
        strip.text.x = element_text(size = 25),legend.text =element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))+
  guides(fill= guide_legend(nrow = 2))
plot(g2)

g3 <- (g2 +  g1)
plot(g3)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/削減量.png", plot=g3,width = 12.85, height = 6.75)

p <- df_red_R20%>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  ggplot(aes(x=REMF,y=IAMC_templatePlus,fill=cate)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  theme_bw()+
  #scale_linetype_manual(values = linetype_bar) + 
  scale_fill_manual(values = color_bar)+
  theme(plot.title = element_blank(), plot.tag=element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size =17.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_blank(), panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),panel.border = element_blank(),
        strip.background = element_blank())
plot(p)

g3 <- ((g2 + g1 )/ p) + plot_layout(widths = c(1,1), heights = c(1, 0.00015))
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/削減量.png", plot=g3,width = 12.85, height = 7.00)

#上位50か国----------------------------------------------------------------------------------------------------------------------------------
df_red_R50 <- df_red_cate %>%
  filter(REMF %in% c("IND","USA","BRA","CHN","PAK","CAN","RUS","NGA","AUS","ETH",
                     "ARG","BGD","TZA","IDN","KEN","MEX","ZAF","IRN","TCD","MLI",
                     "SOM","COL","TUR","FRA","UGA","MDG","THA","MMR","VNM","DEU",
                     "KAZ","BFA","NER","COD","ESP","NPL","NZL","UKR","AFG","DZA",
                     "PHL","VEN","PER","CMR","EGY","ZWE","GBR","POL","ITA","BOL"))%>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  ) %>% 
  filter(YEMF %in% c("2100")) %>% 
  mutate(REMF = recode(REMF,
                       "IND"="India",
                       "USA"="United States",
                       "BRA"="Brazil",
                       "CHN"="China",
                       "PAK"="Pakistan",
                       "CAN"="Canada",
                       "RUS"="Russia",
                       "NGA"="Nigeria",
                       "AUS"="Australia",
                       "ETH"="Ethiopia",
                       "ARG"="Argentina",
                       "BGD"="Bangladesh",
                       "TZA"="Tanzania",
                       "IDN"="Indonesia",
                       "KEN"="Kenya",
                       "MEX"="Mexico",
                       "ZAF"="South Africa",
                       "IRN"="Iran",
                       "TCD"="Chad",
                       "MLI"="Mali",
                       "SOM"="Somalia",
                       "COL"="Colombia",
                       "TUR"="Turkey",
                       "FRA"="France",
                       "UGA"="Uganda",
                       "MDG"="Madagascar",
                       "THA"="Thailand",
                       "MMR"="Myanmar",
                       "VNM"="Vietnam",
                       "DEU"="Germany",
                       "KAZ"="Kazakhstan",
                       "BFA"="Burkina Faso",
                       "NER"="Niger",
                       "COD"="Congo, DR",
                       "ESP"="Spain",
                       "NPL"="Nepal",
                       "NZL"="New Zealand",
                       "UKR"="Ukraine",
                       "AFG"="Afghanistan",
                       "DZA"="Algeria",
                       "PHL"="Philippines",
                       "VEN"="Venezuela",
                       "PER"="Peru",
                       "CMR"="Cameroon",
                       "EGY"="Egypt",
                       "ZWE"="Zimbabwe",
                       "GBR"="United Kingdom",
                       "POL"="Poland",
                       "ITA"="Italy",
                       "BOL"="Bolivia"
  ))

world_data_pop <- world_data_2100[!is.na(world_data_2100$pop_est), ]
world_data_pop<- world_data_pop %>% filter(!is.na(Value))
world_data_pop_50 <- world_data_pop %>%
  group_by(iso_a3_eh) %>%
  summarize(Value = sum((Value)*1000000)/sum((pop_est)))%>%
  filter(iso_a3_eh %in% c( "IND","USA","BRA","CHN","PAK","CAN","RUS","NGA","AUS","ETH",
                           "ARG","BGD","TZA","IDN","KEN","MEX","ZAF","IRN","TCD","MLI",
                           "SOM","COL","TUR","FRA","UGA","MDG","THA","MMR","VNM","DEU",
                           "KAZ","BFA","NER","COD","ESP","NPL","NZL","UKR","AFG","DZA",
                           "PHL","VEN","PER","CMR","EGY","ZWE","GBR","POL","ITA","BOL"))

color_bar <- c("LULUCF(CDR)"="#A5D6A7","LULUCF(その他)"="#90CAF9","農業"="#F4B400")

g4 <- df_red_R50 %>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  mutate(REMF = factor(REMF, levels = c("Bolivia","Italy","Poland","United Kingdom","Zimbabwe","Egypt","Cameroon","Peru","Venezuela","Philippines",
                                        "Algeria","Afghanistan","Ukraine","New Zealand","Nepal","Spain","Congo, DR","Niger","Burkina Faso","Kazakhstan",
                                        "Germany","Vietnam","Myanmar","Thailand","Madagascar","Uganda","France","Turkey","Colombia","Somalia",
                                        "Mali","Chad","Iran","South Africa","Mexico","Kenya","Indonesia","Tanzania","Bangladesh","Argentina",
                                        "Ethiopia","Australia","Nigeria","Russia","Canada","Pakistan","China","Brazil","United States","India"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=IAMC_templatePlus,fill=cate),stat = "identity",width = 0.8,position = "stack")+
  coord_flip() +
  labs(title = "", x="", y = expression(paste('Mitigation potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_text(size = 16.25,color="black",hjust = 0.5,margin = margin(r = 17)), 
        axis.title.x = element_text(size=17), axis.title.y = element_text(size=17),
        strip.text.x = element_text(size = 25),legend.text = element_text(size =15.50),
        legend.position = c(0.80, 0.150),legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),axis.ticks.y = element_blank() ,
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))
#guides(fill= guide_legend(nrow = 2))
plot(g4)

g5 <- world_data_pop_50 %>%
  #mutate(VEMF = factor(VEMF, levels = c("Plantation", "Reforestation", "Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration", "Agroforestry", "Biochar", "Soil carbon sequestration"))) %>%
  mutate(iso_a3_eh = factor(iso_a3_eh, levels = c( "BOL","ITA","POL","GBR","ZWE","EGY","CMR","PER","VEN","PHL",
                                                   "DZA","AFG","UKR","NZL","NPL","ESP","COD","NER","BFA","KAZ",
                                                   "DEU","VNM","MMR","THA","MDG","UGA","FRA","TUR","COL","SOM",
                                                   "MLI","TCD","IRN","ZAF","MEX","KEN","IDN","TZA","BGD","ARG",
                                                   "ETH","AUS","NGA","RUS","CAN","PAK","CHN","BRA","USA","IND"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot(aes(y = iso_a3_eh, x = Value, fill = iso_a3_eh)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8,fill="grey50") +
  #coord_flip() +
  labs(title = "", x=expression(paste('Mitigation intensity (t',{CO[2]},'eq/cap/yr)',seq="")), y = "")+
  theme_bw()+
  #scale_fill_manual(values = color_bar)+
  scale_x_continuous(labels = function(x) scales::comma(abs(x)))+
  scale_x_reverse()+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size = 17),
        strip.text.x = element_text(size = 25),legend.text =element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))+
  guides(fill= guide_legend(nrow = 2))
plot(g5)

g6 <- (g5 +  g4)
plot(g6)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/削減量_50.png", plot=g6,width = 15.25, height = 12.85)

#上位30か国----------------------------------------------------------------------------------------------------------------------------------
df_red_R30_2100 <- df_red_cate %>%
  filter(REMF %in% c("IND","USA","BRA","CHN","PAK","CAN","RUS","NGA","AUS","ETH",
                     "ARG","BGD","TZA","IDN","KEN","MEX","ZAF","IRN","TCD","MLI",
                     "SOM","COL","TUR","FRA","UGA","MDG","THA","MMR","VNM","DEU"))%>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  ) %>% 
  filter(YEMF %in% c("2100")) %>% 
  mutate(REMF = recode(REMF,
                       "IND"="India",
                       "USA"="United States",
                       "BRA"="Brazil",
                       "CHN"="China",
                       "PAK"="Pakistan",
                       "CAN"="Canada",
                       "RUS"="Russia",
                       "NGA"="Nigeria",
                       "AUS"="Australia",
                       "ETH"="Ethiopia",
                       "ARG"="Argentina",
                       "BGD"="Bangladesh",
                       "TZA"="Tanzania",
                       "IDN"="Indonesia",
                       "KEN"="Kenya",
                       "MEX"="Mexico",
                       "ZAF"="South Africa",
                       "IRN"="Iran",
                       "TCD"="Chad",
                       "MLI"="Mali",
                       "SOM"="Somalia",
                       "COL"="Colombia",
                       "TUR"="Turkey",
                       "FRA"="France",
                       "UGA"="Uganda",
                       "MDG"="Madagascar",
                       "THA"="Thailand",
                       "MMR"="Myanmar",
                       "VNM"="Vietnam",
                       "DEU"="Germany"
  ))

world_data_pop <- world_data_2100[!is.na(world_data_2100$pop_est), ]
world_data_pop<- world_data_pop %>% filter(!is.na(Value))
world_data_pop_30 <- world_data_pop %>%
  group_by(iso_a3_eh) %>%
  summarize(Value = sum((Value)*1000000)/sum((pop_est)))%>%
  filter(iso_a3_eh %in% c( "IND","USA","BRA","CHN","PAK","CAN","RUS","NGA","AUS","ETH",
                           "ARG","BGD","TZA","IDN","KEN","MEX","ZAF","IRN","TCD","MLI",
                           "SOM","COL","TUR","FRA","UGA","MDG","THA","MMR","VNM","DEU"))

df_red_R30_2050 <- df_red_cate %>%
  filter(REMF %in% c( "IND","CHN","USA","BRA","PAK","BGD","IDN","CAN","RUS","ARG",
                      "NGA","ETH","THA","VNM","MMR","MEX","AUS","TZA","IRN","ZAF",
                      "TUR","FRA","COL","KEN","DEU","PHL","MDG","MLI","TCD","UGA"))%>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  ) %>% 
  filter(YEMF %in% c("2050")) %>% 
  mutate(REMF = recode(REMF,
                       "IND"="India",
                       "CHN"="China",
                       "USA"="United States",
                       "BRA"="Brazil",
                       "PAK"="Pakistan",
                       "BGD"="Bangladesh",
                       "IDN"="Indonesia",
                       "CAN"="Canada",
                       "RUS"="Russia",
                       "ARG"="Argentina",
                       "NGA"="Nigeria",
                       "ETH"="Ethiopia",
                       "THA"="Thailand",
                       "VNM"="Vietnam",
                       "MMR"="Myanmar",
                       "MEX"="Mexico",
                       "AUS"="Australia",
                       "TZA"="Tanzania",
                       "IRN"="Iran",
                       "ZAF"="South Africa",
                       "TUR"="Turkey",
                       "FRA"="France",
                       "COL"="Colombia",
                       "KEN"="Kenya",
                       "DEU"="Germany",
                       "PHL"="Philippines",
                       "MDG"="Madagascar",
                       "MLI"="Mali",
                       "TCD"="Chad",
                       "UGA"="Uganda"
  ))

world_data_pop_2050 <- world_data_2050[!is.na(world_data_2100$pop_est), ]
world_data_pop_2050<- world_data_pop_2050 %>% filter(!is.na(Value))
world_data_pop_30_2050 <- world_data_pop_2050 %>%
  group_by(iso_a3_eh) %>%
  summarize(Value = sum((Value)*1000000)/sum((pop_est)))%>%
  filter(iso_a3_eh %in% c(  "IND","CHN","USA","BRA","PAK","BGD","IDN","CAN","RUS","ARG",
                            "NGA","ETH","THA","VNM","MMR","MEX","AUS","TZA","IRN","ZAF",
                            "TUR","FRA","COL","KEN","DEU","PHL","MDG","MLI","TCD","UGA"))


color_bar <- c("LULUCF(CDR)"="#A5D6A7","LULUCF(その他)"="#90CAF9","農業"="#F4B400")

g7 <- df_red_R30_2100 %>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  mutate(REMF = factor(REMF, levels = c("Germany","Vietnam","Myanmar","Thailand","Madagascar","Uganda","France","Turkey","Colombia","Somalia",
                                        "Mali","Chad","Iran","South Africa","Mexico","Kenya","Indonesia","Tanzania","Bangladesh","Argentina",
                                        "Ethiopia","Australia","Nigeria","Russia","Canada","Pakistan","China","Brazil","United States","India"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=IAMC_templatePlus,fill=cate),stat = "identity",width = 0.8,position = "stack")+
  coord_flip() +
  labs(title = "", x="", y = expression(paste('Mitigation potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x = element_text(size = 17,color="black"), axis.text.y = element_text(size = 20.25,color="black",hjust = 0.5,margin = margin(r = 17)), 
        axis.title.x = element_text(size=22), axis.title.y = element_text(size=22),
        strip.text.x = element_text(size = 25),legend.text = element_text(size =20.25),
        legend.position = c(0.775, 0.100),legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),axis.ticks.y = element_blank() ,
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))
#guides(fill= guide_legend(nrow = 2))
plot(g7)

g8 <- world_data_pop_30 %>%
  #mutate(VEMF = factor(VEMF, levels = c("Plantation", "Reforestation", "Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration", "Agroforestry", "Biochar", "Soil carbon sequestration"))) %>%
  mutate(iso_a3_eh = factor(iso_a3_eh, levels = c( "DEU","VNM","MMR","THA","MDG","UGA","FRA","TUR","COL","SOM",
                                                   "MLI","TCD","IRN","ZAF","MEX","KEN","IDN","TZA","BGD","ARG",
                                                   "ETH","AUS","NGA","RUS","CAN","PAK","CHN","BRA","USA","IND"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot(aes(y = iso_a3_eh, x = Value, fill = iso_a3_eh)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8,fill="grey50") +
  #coord_flip() +
  labs(title = "", x=expression(paste('Mitigation intensity (t',{CO[2]},'eq/cap/yr)',seq="")), y = "")+
  theme_bw()+
  #scale_fill_manual(values = color_bar)+
  scale_x_continuous(labels = function(x) scales::comma(abs(x)))+
  scale_x_reverse()+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 17,color="black"), axis.text.y = element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_text(size=22), axis.title.y = element_text(size = 22),
        strip.text.x = element_text(size = 25),legend.text =element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))+
  guides(fill= guide_legend(nrow = 2))
plot(g8)

g9 <- (g8 +  g7)
plot(g9)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/削減量_30_2100.png", plot=g9,width = 15.25, height = 10.25)

g10 <- df_red_R30_2050 %>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  mutate(REMF = factor(REMF, levels = c( "Uganda","Chad","Mali","Madagascar","Philippines","Germany","Kenya","Colombia","France","Turkey",
                                         "South Africa","Iran","Tanzania","Australia","Mexico","Myanmar","Vietnam","Thailand","Ethiopia","Nigeria",
                                         "Argentina","Russia","Canada","Indonesia","Bangladesh","Pakistan","Brazil","United States","China","India"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=IAMC_templatePlus,fill=cate),stat = "identity",width = 0.8,position = "stack")+
  coord_flip() +
  labs(title = "", x="", y = expression(paste('Mitigation potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_text(size = 16.25,color="black",hjust = 0.5,margin = margin(r = 17)), 
        axis.title.x = element_text(size=20), axis.title.y = element_text(size=20),
        strip.text.x = element_text(size = 25),legend.text = element_text(size =15.50),
        legend.position = c(0.80, 0.150),legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),axis.ticks.y = element_blank() ,
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))
#guides(fill= guide_legend(nrow = 2))
plot(g10)

g11 <- world_data_pop_30_2050 %>%
  #mutate(VEMF = factor(VEMF, levels = c("Plantation", "Reforestation", "Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration", "Agroforestry", "Biochar", "Soil carbon sequestration"))) %>%
  mutate(iso_a3_eh = factor(iso_a3_eh, levels = c("UGA","TCD","MLI","MDG","PHL","DEU","KEN","COL","FRA","TUR",
                                                  "ZAF","IRN","TZA","AUS","MEX","MMR","VNM","THA","ETH","NGA",
                                                  "ARG","RUS","CAN","IDN","BGD","PAK","BRA","USA","CHN","IND"))) %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot(aes(y = iso_a3_eh, x = Value, fill = iso_a3_eh)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8,fill="grey50") +
  #coord_flip() +
  labs(title = "", x=expression(paste('Mitigation intensity (t',{CO[2]},'eq/cap/yr)',seq="")), y = "")+
  theme_bw()+
  #scale_fill_manual(values = color_bar)+
  scale_x_continuous(labels = function(x) scales::comma(abs(x)))+
  scale_x_reverse()+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_text(size=17), axis.title.y = element_text(size = 17),
        strip.text.x = element_text(size = 25),legend.text =element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(0, 0, 0, 0))+
  guides(fill= guide_legend(nrow = 2))
plot(g11)

g12 <- (g11 +  g10)
plot(g12)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/削減量_30_2050.png", plot=g12,width = 15.25, height = 8.85)
