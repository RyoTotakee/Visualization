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

df_set_EN <- df_set %>%
  mutate(VEMF=recode(VEMF,Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre="High genetic merit",Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee="Replacement of roughage with concentrates",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad="Covered anaerobic digesters",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And="Anaerobic Digestion -Centralised plant",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl="Dome digester",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc="Aerobic decomposition",
                     Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams="Ammonium sulfate",Emi_Red_equ_CH4_AFO_Agr_Ric_Wat="Midseason drainage",Emi_Red_equ_CH4_AFO_Agr_Ric_Res="Off-season incorporation",
                     Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til="no-tillage",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer="High efficiency fertilizer application",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res="Replacing fertilizer with manure-N and residue",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo="Use of slow-release type fertilizers",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin="Nitrification Inhibitors",
                     Emi_Red_CO2_AFO_Lan_Pla="Afforestation",Emi_Red_CO2_AFO_Lan_Ref="Reforestation",Emi_Red_CO2_AFO_Lan_FPr="Forest protection",Emi_Red_CO2_AFO_Lan_Pff="Prevention of forest fire","Emi_Red_CO2_AFO_Lan_Ril"="Reduced Impact logging",
                     Emi_Red_CO2_AFO_Lan_Enr="Enhanced natural regeneration",Emi_Red_CO2_AFO_Lan_Agf="Agroforestry",Emi_Red_CO2_AFO_Lan_Bch="Biochar",Emi_Red_CO2_AFO_Lan_Scs="Soil carbon sequestration"))
df_set_EN <- df_set_EN %>%
  mutate(IAMC_templatePlus = ifelse(IAMC_templatePlus < 0, -IAMC_templatePlus, IAMC_templatePlus))


#対策技術別削減量(5地域)-------------------------------------------------------------
df_category <- df_set %>%
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

df_category_Top <- filter(df_category, REMF %in% c("USA","IND"))
df_category_Top <-df_category_Top %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )%>%
  filter(YEMF %in% c("2100"))

df_category_R5 <- filter(df_category, REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))
df_category_R5 <- df_category_R5 %>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU"))
df_category_R5_sum <-df_category_R5 %>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_category_R5_2100 <- filter(df_category_R5, YEMF %in% c("2100"))

df_category_R5_main <-df_category_R5_2100 %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )


color_bar_c <- c(
  "植林" = "#66c2a5",                # 緑青
  "再植林" = "#8da0cb",              # ラベンダーブルー
  "森林保護" = "#1b7837",            # 濃緑
  "森林火災の防止" = "#e41a1c",     # 赤（警戒色）
  "環境負荷の少ない伐採" = "#a65628", # 茶色
  "自然再生の強化" = "#4daf4a",      # 明るい緑
  
  "アグロフォレストリー" = "#bf812d", # 緑青（植林と同系色）
  "バイオ炭" = "#b3b3b3",              # 中間グレー（炭）
  "土壌炭素隔離" = "#a6d854",          # 黄緑（自然的）
  
  "生産性が高い家畜種への変更" = "#fc8d62", # オレンジ
  "濃厚飼料の投与"="#A52A2A",
  "糞尿堆肥化" = "#e78ac3",              # ピンク
  "窒素肥料の硫酸アンモニウムへの変更" = "#80b1d3",  # 明るい水色
  "ラグーンカバー"="lightgreen",
  "水田への間断灌漑の導入" = "#b3de69", # ライトグリーン（農業）
  "不耕起移植栽培" = "#ffd92f",         # 黄色
  "肥料の高効率利用"="#984ea3",
  "化学肥料の有機肥料による置き換え" = "#e5c494", # 薄茶色
  "硝化抑制剤を含む肥料の利用" = "#377eb8"       # 青
)

g1 <- df_category_R5_2100 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(cate = factor(cate, levels = c("農業","森林再生", "保護", "管理", "農地再生"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("生産性が高い家畜種への変更", "濃厚飼料の投与","糞尿堆肥化", 
                                        "窒素肥料の硫酸アンモニウムへの変更", "水田への間断灌漑の導入", "不耕起移植栽培","肥料の高効率利用",
                                        "化学肥料の有機肥料による置き換え", "硝化抑制剤を含む肥料の利用",
                                        "植林", "再植林", "森林保護",
                                        "環境負荷の少ない伐採", "自然再生の強化","森林火災の防止","アグロフォレストリー", 
                                        "バイオ炭", "土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=cate,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",width = 0.8,position = "stack",color="black",size = 0.15)+
  labs(title = "", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar_c) +
  #scale_pattern_manual(values = c("none","stripe", "circle", "crosshatch", "stripe", "circle", "crosshatch","stripe", "circle", "crosshatch","none", "circle", "crosshatch","stripe", "circle", "none")) + 
  #scale_pattern_density_manual(values = c(0.05, 0.0020, 0.0020, 0.0020, 0.20, 0.40, 0.20, 0.015, 0.025, 0.025, 0.070, 0.070, 0.070, 0.085, 0.270, 0.050)) + 
  #scale_pattern_spacing_manual(values = c(0.05, 0.0141, 0.0171, 0.0141, 0.01, 0.05, 0.02, 0.028, 0.050, 0.025, 0.015, 0.012, 0.015, 0.025, 0.025, 0.025)) +  
  #scale_fill_manual(values = color_bar)+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 18.25,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 17.5,color="black"), 
        axis.title.x = element_text(size=17.5), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =21.75),
        legend.position = "right",legend.title = element_blank(),legend.direction = "vertical" ,
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(ncol = 1)) 
plot(g1)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/対策技術別削減量.png", plot=g1, width = 15.25, height = 7.50)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/PPT/対策技術別削減量.png", plot=g1, width = 14.5, height = 6.75)

df_category_EN <- df_set_EN %>%
  mutate(cate = case_when(
    VEMF == "Afforestation" ~ "Forest Restore",
    VEMF == "Reforestation" ~ "Forest Restore",
    VEMF == "Forest protection" ~ "Protect",
    VEMF == "Reduced Impact logging" ~ "Protect",
    VEMF == "Enhanced natural regeneration" ~ "Manage",
    VEMF == "Prevention of forest fire" ~ "Manage",
    VEMF == "Agroforestry" ~ "Restore",
    VEMF == "Biochar" ~ "Restore",
    VEMF == "Soil carbon sequestration" ~ "Restore",
    VEMF == "High genetic merit" ~ "Agr.",
    VEMF == "Replacement of roughage with concentrates" ~ "Agr.",
    VEMF == "Covered anaerobic digesters" ~ "Agr.",
    VEMF == "Anaerobic Digestion -Centralised plant" ~ "Agr.",
    VEMF == "Dome digester" ~ "Agr.",
    VEMF == "Aerobic decomposition" ~ "Agr.",
    VEMF == "Ammonium sulfate" ~ "Agr.",
    VEMF == "Midseason drainage" ~ "Agr.",
    VEMF == "Off-season incorporation" ~ "Agr.",
    VEMF == "no-tillage" ~ "Agr.",
    VEMF == "High efficiency fertilizer application" ~ "Agr.",
    VEMF == "Use of slow-release type fertilizers" ~ "Agr.",
    VEMF == "Replacing fertilizer with manure-N and residue" ~ "Agr.",
    VEMF == "Nitrification Inhibitors" ~ "Agr."
  ))

df_category_R5_EN <- filter(df_category_EN, REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))
df_category_R5_EN <- df_category_R5_EN %>%
  mutate(REMF=recode(REMF,R5REF="Former soviet union",R5LAM="Latin America",R5ASIA="Asia",R5MAF="Africa / Middle east","R5OECD90+EU"="OECD+EU"))
df_category_R5_2100_EN <- filter(df_category_R5_EN, YEMF %in% c("2100"))

color_bar_en <- c(
  "Afforestation" = "#66c2a5",                # 緑青
  "Reforestation" = "#8da0cb",              # ラベンダーブルー
  "Forest protection" = "#1b7837",            # 濃緑
  "Prevention of forest fire" = "#e41a1c",     # 赤（警戒色）
  "Reduced Impact logging" = "#a65628", # 茶色
  "Enhanced natural regeneration" = "#4daf4a",      # 明るい緑
  
  "Agroforestry" = "#bf812d", # 緑青（植林と同系色）
  "Biochar" = "#b3b3b3",              # 中間グレー（炭）
  "Soil carbon sequestration" = "#a6d854",          # 黄緑（自然的）
  
  "High genetic merit" = "#fc8d62", # オレンジ
  "Replacement of roughage with concentrates"="#A52A2A",
  #"Covered anaerobic digesters"="lightgreen",
  "Aerobic decomposition" = "#e78ac3",              # ピンク
  "Ammonium sulfate" = "#80b1d3",  # 明るい水色
  
  "Midseason drainage" = "#b3de69", # ライトグリーン（農業）
  "no-tillage" = "#ffd92f",         # 黄色
  "High efficiency fertilizer application"="#984ea3",
  "Replacing fertilizer with manure-N and residue" = "#e5c494", # 薄茶色
  "Nitrification Inhibitors" = "#377eb8"       # 青
)

g1_en <- df_category_R5_2100_EN %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","Asia","Africa / Middle east","Latin America","Former soviet union"))) %>%
  mutate(cate = factor(cate, levels = c("Agr.","Forest Restore", "Protect", "Manage", "Restore"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("High genetic merit", "Replacement of roughage with concentrates", "Aerobic decomposition",
                                        "Ammonium sulfate", "Midseason drainage", "no-tillage",
                                        "High efficiency fertilizer application", "Use of slow-release type fertilizers",
                                        "Replacing fertilizer with manure-N and residue", "Nitrification Inhibitors", "Afforestation",
                                        "Reforestation", "Forest protection", "Reduced Impact logging",
                                        "Enhanced natural regeneration", "Prevention of forest fire", "Agroforestry",
                                        "Biochar", "Soil carbon sequestration"))) %>%
  ggplot() +
  geom_bar(aes(x=cate,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",width = 0.8,position = "stack",color="black",size = 0.15)+
  labs(title = "", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar_en,
                    labels = function(x) stringr::str_wrap(x, width = 32.75)) +
  #scale_pattern_manual(values = c("none","stripe", "circle", "crosshatch", "stripe", "circle", "crosshatch","stripe", "circle", "crosshatch","none", "circle", "crosshatch","stripe", "circle", "none")) + 
  #scale_pattern_density_manual(values = c(0.05, 0.0020, 0.0020, 0.0020, 0.20, 0.40, 0.20, 0.015, 0.025, 0.025, 0.070, 0.070, 0.070, 0.085, 0.270, 0.050)) + 
  #scale_pattern_spacing_manual(values = c(0.05, 0.0141, 0.0171, 0.0141, 0.01, 0.05, 0.02, 0.028, 0.050, 0.025, 0.015, 0.012, 0.015, 0.025, 0.025, 0.025)) +  
  #scale_fill_manual(values = color_bar)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 24.5)) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 17.55,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 17.5,color="black"), 
        axis.title.x = element_text(size=17.5), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =20.75),
        legend.position = "right",legend.title = element_blank(),legend.direction = "vertical" ,
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(ncol = 1)) 
plot(g1_en)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/PPT/対策技術別削減量_en.png", plot=g1_en, width = 14.5, height = 7.25)

#削減量(各国レベル)-------------------------------------------------------------
df_red_R178 <- df_set %>%
  filter(!REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))
df_red_R178_sum <- df_red_R178 %>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )
df_red_R178_2100 <- df_red_R178_sum %>%
  filter(YEMF %in% c("2100"))

df_red_R178_2050 <- df_red_R178_sum %>%
  filter(YEMF %in% c("2050"))

df_red_R5 <- df_set %>%
  filter(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World")) %>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )%>%
  filter(YEMF %in% c("2100"))

df_red_GL <- df_set %>%
  filter(REMF %in% c("World")) %>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )

world_set <- ne_countries(scale = "medium", returnclass = "sf")
world_set2 <- world_set %>%
  filter(!name == "Antarctica")
world <- world_set2 %>%
  filter(name != "Greenland")

world_data_2100 <- world %>%
  left_join(df_red_R178_2100, by = c("iso_a3_eh" = "REMF"))

world_data_2050 <- world %>%
  left_join(df_red_R178_2050, by = c("iso_a3_eh" = "REMF"))

world_data_eco <- world_data_2100[, c("economy", "sovereignt","Value","YEMF")]
sum_eco <- world_data_eco %>%
  group_by(YEMF,economy) %>%
  summarize(value = (sum(Value)))

world_na <- world_data_2100 %>% 
  filter(is.na(Value))
world_with_data <- world_data_2100 %>% 
  filter(!is.na(Value))

g2<-
  ggplot() +
  #geom_sf(aes(fill = new)) +
  geom_sf(data = world_with_data, aes(fill = Value/1000), color = "black") +
  geom_sf(data = world_na, fill = "white", color = "black", size = 0.4) +
  scale_fill_gradient(name = expression(paste('Potential (Gt',{CO[2]},'eq/yr)',seq="")),low = "#fff9f3", high = "#d94801") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag = "(a)")+
  guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =11.25),
        legend.position = c(0.10, 0.150),legend.title = element_text(size =12.75,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
plot(g2)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/PPT/2100_potential.png", plot=g2, width = 10.15, height = 4.6)

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

df_red_cate_R5 <- filter(df_red_cate, REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))
df_red_cate_R5 <- df_red_cate_R5 %>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) 
df_red_cate_R5_2100 <- filter(df_red_cate_R5, YEMF %in% c("2100"))
df_red_cate_R5_2100_sum <- df_red_cate_R5_2100 %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

df_red_cate_GL <- filter(df_red_cate, REMF %in% c("World"))
df_red_cate_GL_sum <- df_red_cate_GL %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  ) %>% 
  filter(YEMF %in% c("2030","2050","2100"))

color_bar <- c("LULUCF(CDR)"="#A5D6A7","LULUCF(その他)"="#90CAF9","農業"="#F4B400")

g3 <- df_red_cate_R5_2100_sum %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=IAMC_templatePlus/1000,fill=cate),stat = "identity")+
  #facet_wrap(~REMF, nrow = 5, scales = "free_y")+
  #annotate("text", x = -Inf, y = Inf, label = "(b)", hjust = -0.2, vjust = 1.5, size = 5)
  labs(title = "", tag="(b)",x="", y = expression(paste('Mitigation potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar,
                    labels = c(
                      
                      "LULUCF\n(CDR)",         # 改行：LULUCF (CDR)
                      "LULUCF\n(その他)",      # 改行：LULUCF (その他)
                      "農業"
                    ))+
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_text(size = 12.75,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size = 15.00),strip.text.y = element_text(size = 15.00),legend.text = element_text(size =15),
        legend.position = "right",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),axis.ticks = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
#guides(fill = guide_legend(nrow = 2))
plot(g3)

g4_1 <-(g2 | g3) + plot_layout(widths = c(5, 1), heights = c(1, 1))
plot(g4_1)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/2100_potential.png", plot=g4_1, width = 11.15, height = 4.6)

g3_1 <- df_red_cate_GL_sum %>%
  mutate(cate = factor(cate, levels = c("LULUCF(CDR)","LULUCF(その他)","農業"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=cate),stat = "identity")+
  #facet_wrap(~REMF, nrow = 5, scales = "free_y")+
  #annotate("text", x = -Inf, y = Inf, label = "(b)", hjust = -0.2, vjust = 1.5, size = 5)
  labs(tag = "(a)", x="", y = expression(paste('Mitigation potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar)+
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 15,color="black"), axis.text.y = element_text(size = 12.75,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=15),
        strip.text.x = element_text(size = 15.00),strip.text.y = element_text(size = 15.00),legend.text = element_text(size =15),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),axis.ticks = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
#guides(fill = guide_legend(nrow = 2))
plot(g3_1)

g3_2 <-(g3_1 | g3) + plot_layout(widths = c(1, 2), heights = c(1, 1))
plot(g3_2)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/世界全体.png", plot=g3_2, width = 11.15, height = 4.6)


#削減量推移(各国レベル)-------------------------------------------------------------
df_red_TOP50 <- df_set %>%
  filter(REMF %in% c("IND","USA","BRA","CHN","PAK","CAN","NGA","AUS","RUS","ETH",
                     "ARG","BGD","TZA","KEN","IDN","MEX","ZAF","IRN","TCD","MLI",
                     "SOM","UGA","FRA","TUR","COL","MDG","THA","MMR","VNM","DEU",
                     "KAZ","BFA","NER","NPL","ESP","NZL","COD","UKR","AFG","DZA",
                     "PHL","VEN","PER","CMR","EGY","ZWE","GBR","POL","ITA","BOL")) 
df_red_TOP50_cate <- df_red_TOP50 %>%
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


df_red_TOP50_cate_sum <- df_red_TOP50_cate %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

color_bar <- c("LULUCF(CDR)"="#A5D6A7","LULUCF(その他)"="#ADD8E6","農業"="#FFCA28")

color_bar <- c(
  "LULUCF(CDR)" = "#A5D6A7",   # 緑（そのまま）
  "LULUCF(その他)" = "#90CAF9", # 明るめで爽やかな青（#ADD8E6 より少し濃い）
  "農業" = "#F4B400"           # 黄金に近い落ち着いた黄色（Google系で見やすい）
)

g4 <- df_red_TOP50_cate_sum %>%
  mutate(REMF = factor(REMF, levels = c("IND","USA","BRA","CHN","PAK","CAN","NGA","AUS","RUS","ETH",
                                        "ARG","BGD","TZA","KEN","IDN","MEX","ZAF","IRN","TCD","MLI",
                                        "SOM","UGA","FRA","TUR","COL","MDG","THA","MMR","VNM","DEU",
                                        "KAZ","BFA","NER","NPL","ESP","NZL","COD","UKR","AFG","DZA",
                                        "PHL","VEN","PER","CMR","EGY","ZWE","GBR","POL","ITA","BOL"))) %>%
  #mutate(applicability = factor(applicability, levels = c("nonlim", "lim_var", "lim_const"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=cate),stat = "identity")+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 10, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar)+
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
  #guides(fill = guide_legend(nrow = 3))+
  xlab(NULL)
plot(g4)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_TOP50.png", plot=g4, width = 16.75, height = 16.00)

df_red_TOP50_sub <- df_red_TOP50 %>%
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

df_red_TOP50_sub_sum <- df_red_TOP50_sub %>%
  group_by(REMF,YEMF,sub) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8",
  "LULUCF(CDR)" = "#A5D6A7",
  "LULUCF(その他)" = "#90CAF9"
)

g5 <- df_red_TOP50_sub_sum %>%
  mutate(REMF = factor(REMF, levels = c("IND","USA","BRA","CHN","PAK","CAN","NGA","AUS","RUS","ETH",
                                        "ARG","BGD","TZA","KEN","IDN","MEX","ZAF","IRN","TCD","MLI",
                                        "SOM","UGA","FRA","TUR","COL","MDG","THA","MMR","VNM","DEU",
                                        "KAZ","BFA","NER","NPL","ESP","NZL","COD","UKR","AFG","DZA",
                                        "PHL","VEN","PER","CMR","EGY","ZWE","GBR","POL","ITA","BOL"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 10, scales = "free_y")+
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
plot(g5)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/MitPot_TOP50_sub.png", plot=g5, width = 16.75, height = 16.00)

#CDR----------------------------------------------------------------------------------------------------------------------------------
df_red_R178 <- df_set %>%
  filter(!REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))
df_agf <- df_red_R178 %>%
  filter(VEMF %in% c("アグロフォレストリー")) %>%
  filter(YEMF %in% c("2100"))
df_bch <- df_red_R178 %>%
  filter(VEMF %in% c("バイオ炭")) %>%
  filter(YEMF %in% c("2100"))
df_soc<- df_red_R178 %>%
  filter(VEMF %in% c("土壌炭素隔離")) %>%
  filter(YEMF %in% c("2100"))
df_afr<- df_red_R178 %>%
  filter(VEMF %in% c("植林")) %>%
  filter(YEMF %in% c("2100"))

df_red_CDR <- df_set %>%
  filter(REMF %in% c("World"))%>%
  filter(VEMF %in% c("植林","アグロフォレストリー","バイオ炭","土壌炭素隔離")) %>%
  filter(YEMF %in% c("2100"))

world_set <- ne_countries(scale = "medium", returnclass = "sf")
world_set2 <- world_set %>%
  filter(!name == "Antarctica")
world <- world_set2 %>%
  filter(name != "Greenland")

world_data_agf <- world %>%
  left_join(df_agf, by = c("iso_a3_eh" = "REMF"))
world_data_bch <- world %>%
  left_join(df_bch, by = c("iso_a3_eh" = "REMF"))
world_data_soc <- world %>%
  left_join(df_soc, by = c("iso_a3_eh" = "REMF"))
world_data_afr <- world %>%
  left_join(df_afr, by = c("iso_a3_eh" = "REMF"))

world_agf_na <- world_data_agf %>% 
  filter(is.na(IAMC_templatePlus))
world_agf_with_data <- world_data_agf %>% 
  filter(!is.na(IAMC_templatePlus))

world_bch_na <- world_data_bch %>% 
  filter(is.na(IAMC_templatePlus))
world_bch_with_data <- world_data_bch %>% 
  filter(!is.na(IAMC_templatePlus))

world_soc_na <- world_data_soc %>% 
  filter(is.na(IAMC_templatePlus))
world_soc_with_data <- world_data_soc %>% 
  filter(!is.na(IAMC_templatePlus))

world_afr_na <- world_data_afr %>% 
  filter(is.na(IAMC_templatePlus))
world_afr_with_data <- world_data_afr %>% 
  filter(!is.na(IAMC_templatePlus))

g6<-
  ggplot() +
  #geom_sf(aes(fill = new)) +
  geom_sf(data = world_agf_with_data, aes(fill = IAMC_templatePlus), color = "black") +
  geom_sf(data = world_agf_na, fill = "white", color = "black", size = 0.4) +
  scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#FFF1D6", high = "#B76516") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag = "(a)")+
  guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =10.5),
        legend.position = c(0.08, 0.275),legend.title = element_text(size =11,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),plot.background = element_rect(color = "black", size = 1),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank())
plot(g6)

g7<-
  ggplot() +
  #geom_sf(aes(fill = new)) +
  geom_sf(data = world_bch_with_data, aes(fill = IAMC_templatePlus), color = "black") +
  geom_sf(data = world_bch_na, fill = "white", color = "black", size = 0.4) +
  scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#f0f0f0", high = "#4d4d4d") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag = "(b)")+
  guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =10.5),
        legend.position = c(0.08, 0.275),legend.title = element_text(size =11,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),plot.background = element_rect(color = "black", size = 1),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank())
plot(g7)

g8<-
  ggplot() +
  #geom_sf(aes(fill = new)) +
  geom_sf(data = world_soc_with_data, aes(fill = IAMC_templatePlus), color = "black") +
  geom_sf(data = world_soc_na, fill = "white", color = "black", size = 0.4) +
  scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#FBF8F1", high = "#6E5A2E") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag = "(c)")+
  guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =10.5),
        legend.position = c(0.08, 0.275),legend.title = element_text(size =11,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),plot.background = element_rect(color = "black", size = 1),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank())
plot(g8)

g9<-
  ggplot() +
  #geom_sf(aes(fill = new)) +
  geom_sf(data = world_afr_with_data, aes(fill = IAMC_templatePlus), color = "black") +
  geom_sf(data = world_afr_na, fill = "white", color = "black", size = 0.4) +
  scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#f7fcf5", high = "#4c762e") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag = "(d)")+
  guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =10.5),
        legend.position = c(0.08, 0.275),legend.title = element_text(size =11,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),plot.background = element_rect(color = "black", size = 1),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank())
plot(g9)

g10_1 <- (g6| plot_spacer() |g7) + plot_layout(widths = c(1,  0.00001, 1), heights = c(1, 1))
g10_2 <- (g8| plot_spacer() |g9) + plot_layout(widths = c(1,  0.00001, 1), heights = c(1, 1))
g10 <- (g10_1/  plot_spacer() /g10_2) + plot_layout(widths = c(1,1), heights = c(1, 0.00001, 1))
#+ plot_layout(widths = c(1,  0.0001, 1), heights = c(1, 1))
plot(g10)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/CDR.png", plot=g10,width = 14.5, height = 6.65)

#CDR_TOP50--------------------------------------------------------------------
df_set_CDR <- df_set %>%
  filter(VEMF %in% c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarise(
    IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE)
  ) %>%
  mutate(VEMF = "CDR")

df_CDR_top50 <- df_set_CDR %>%
  filter((YEMF %in% c("2100"))) %>% 
  filter(!(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))) %>% 
  group_by(Scenario,VEMF,YEMF) %>%
  arrange(desc(IAMC_templatePlus)) %>%
  head(50)

df_CDR_top50 <- df_set %>%
  filter((YEMF %in% c("2100"))) %>%
  filter(VEMF %in% c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離")) %>%
  filter((REMF %in% c("USA","CHN","RUS","CAN","BRA","IND","AUS","ARG","KAZ","MEX",
                      "COD","DZA","IDN","IRN","UKR","TCD","TUR","FRA","ETH","MNG",
                      "PER","COL","BOL","LBY","DEU","PAK","VEN","AFG","ESP","POL",
                      "ITA","CAF","NAM","KEN","MMR","MAR","SAU","THA","CIV","CMR",
                      "ZAF","IRQ","PHL","UZB","ZWE","MDG","BGD","GIN","BLR","VNM"))) %>%
  mutate(REMF = recode(REMF,
                       "USA" = "United States",
                       "CHN" = "China",
                       "RUS" = "Russia",
                       "CAN" = "Canada",
                       "BRA" = "Brazil",
                       "IND" = "India",
                       "AUS" = "Australia",
                       "ARG" = "Argentina",
                       "KAZ" = "Kazakhstan",
                       "MEX" = "Mexico",
                       "COD" = "Congo, DR",
                       "DZA" = "Algeria",
                       "IDN" = "Indonesia",
                       "IRN" = "Iran",
                       "UKR" = "Ukraine",
                       "TCD" = "Chad",
                       "TUR" = "Turkey",
                       "FRA" = "France",
                       "ETH" = "Ethiopia",
                       "MNG" = "Mongolia",
                       "PER" = "Peru",
                       "COL" = "Colombia",
                       "BOL" = "Bolivia",
                       "LBY" = "Libya",
                       "DEU" = "Germany",
                       "PAK" = "Pakistan",
                       "VEN" = "Venezuela",
                       "AFG" = "Afghanistan",
                       "ESP" = "Spain",
                       "POL" = "Poland",
                       "ITA" = "Italy",
                       "CAF" = "CAR",
                       "NAM" = "Namibia",
                       "KEN" = "Kenya",
                       "MMR" = "Myanmar",
                       "MAR" = "Morocco",
                       "SAU" = "Saudi Arabia",
                       "THA" = "Thailand",
                       "CIV" = "Côte d'Ivoire",
                       "CMR" = "Cameroon",
                       "ZAF" = "South Africa",
                       "IRQ" = "Iraq",
                       "PHL" = "Philippines",
                       "UZB" = "Uzbekistan",
                       "ZWE" = "Zimbabwe",
                       "MDG" = "Madagascar",
                       "BGD" = "Bangladesh",
                       "GIN" = "Guinea",
                       "BLR" = "Belarus",
                       "VNM" = "Vietnam"
  ))

total2100 <- df_CDR_top50 %>%
  group_by(REMF) %>%
  summarise(total_value = sum(IAMC_templatePlus))
total2100 <- total2100 %>%
  mutate(x = 0, y = 0)

df_CDR_top20 <- df_CDR_top50 %>%
  filter((REMF %in% c("United States","China","Russia","Canada","Brazil",
                      "India","Australia","Argentina","Kazakhstan","Mexico",
                      "Congo, DR","Algeria","Indonesia","Iran","Ukraine",
                      "Chad","Turkey","France","Ethiopia","Mongolia","Peru")))

total2100_top20 <- df_CDR_top20 %>%
  group_by(REMF) %>%
  summarise(total_value = sum(IAMC_templatePlus))
total2100_top20 <- total2100_top20 %>%
  mutate(x = 0, y = 0)

color_bar1 <- c(
  "植林" = "#4C9A2A",
  "アグロフォレストリー" = "#E39C3E",
  "バイオ炭" = "#5E5E5E",
  "土壌炭素隔離" = "#C2A878"
)

g11 <- df_CDR_top50 %>%
  mutate(VEMF = factor(VEMF, levels = c("植林","アグロフォレストリー",
                                        "バイオ炭","土壌炭素隔離"))) %>%
  mutate(REMF = factor(REMF, levels = c("United States","China","Russia","Canada","Brazil","India","Australia",
                                        "Argentina","Kazakhstan","Mexico","Congo, DR","Algeria","Indonesia",
                                        "Iran","Ukraine","Chad","Turkey","France","Ethiopia","Mongolia",
                                        "Peru","Colombia","Bolivia","Libya","Germany","Pakistan","Venezuela",
                                        "Afghanistan","Spain","Poland","Italy","CAR",
                                        "Namibia","Kenya","Myanmar","Morocco","Saudi Arabia","Thailand",
                                        "Côte d'Ivoire","Cameroon","South Africa","Iraq","Philippines",
                                        "Uzbekistan","Zimbabwe","Madagascar","Bangladesh","Guinea",
                                        "Belarus","Vietnam"))) %>%
  ggplot() +
  geom_bar(aes(x="",y=-IAMC_templatePlus,fill=VEMF),stat = "identity",color="black",linewidth = 0.15)+
  #geom_text(aes(x = "", y =-Value/1000, label = label),
  #          position = position_stack(vjust = 0.5),inherit.aes = TRUE,size = 5) +
  geom_text(data = total2100,
            aes(x = x, y = y, label = paste0(round(total_value, 1))),
            inherit.aes = FALSE,
            size = 5.00)+
  labs(title = "",x="", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  #facet_grid(rows = vars(cate), cols = vars(applicability), scales = "free_y")+
  facet_wrap(~REMF, ncol  = 10, scales = "free_y")+
  coord_polar("y") +                        # 極座標で変換（y軸を使う）
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 20.75,color="black"), 
        axis.title.x = element_text(size = 20.75,color="black"), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 15.00),strip.text.y = element_text(size = 15.00),legend.text = element_text(size =20.75),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),axis.ticks = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  #guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g11)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/CDR削減内訳_2100.png", plot=g11, width = 15.85, height = 10.25, units="in", dpi=300)

g11_p <- df_CDR_top20 %>%
  mutate(VEMF = factor(VEMF, levels = c("植林","アグロフォレストリー",
                                        "バイオ炭","土壌炭素隔離"))) %>%
  mutate(REMF = factor(REMF, levels = c("United States","China","Russia","Canada","Brazil",
                                        "India","Australia","Argentina","Kazakhstan","Mexico",
                                        "Congo, DR","Algeria","Indonesia","Iran","Ukraine",
                                        "Chad","Turkey","France","Ethiopia","Mongolia","Peru"))) %>%
  ggplot() +
  geom_bar(aes(x="",y=-IAMC_templatePlus,fill=VEMF),stat = "identity",color="black",linewidth = 0.15)+
  #geom_text(aes(x = "", y =-Value/1000, label = label),
  #          position = position_stack(vjust = 0.5),inherit.aes = TRUE,size = 5) +
  geom_text(data = total2100_top20,
            aes(x = x, y = y, label = paste0(round(total_value, 1))),
            inherit.aes = FALSE,
            size = 5.25)+
  labs(title = "",x="", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  #facet_grid(rows = vars(cate), cols = vars(applicability), scales = "free_y")+
  facet_wrap(~REMF, ncol  = 7, scales = "free_y")+
  coord_polar("y") +                        # 極座標で変換（y軸を使う）
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 20.75,color="black"), 
        axis.title.x = element_text(size = 20.75,color="black"), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 16.00),strip.text.y = element_text(size = 16.00),legend.text = element_text(size =20.75),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),axis.ticks = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  #guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g11_p)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/PPT/CDR削減内訳_2100.png", plot=g11_p, width = 12.85, height = 7.25, units="in", dpi=300)

#CDR_累積値--------------------------------------------------------------------
df_CDR_cum <- df_set %>%
  filter(VEMF %in% c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))%>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF")) %>% 
  #filter(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","CHN","USA","CAN","BRA","IND","TUR","JPN"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>%
  mutate(YEMF = as.numeric(as.character(YEMF)))
  mutate(Sr = case_when(
    REMF == "XE25" ~ "Europe",
    REMF == "XER" ~ "Europe",
    REMF == "TUR" ~ "Middle East",
    REMF == "JPN" ~ "Rest of Asia",
    REMF == "CAN" ~ "North America",
    REMF == "USA" ~ "North America",
    REMF == "CHN" ~ "China",
    REMF == "IND" ~ "India",
    REMF == "XSE" ~ "Rest of Asia",
    REMF == "XSA" ~ "Rest of Asia",
    REMF == "BRA" ~ "Brazil",
    REMF == "XLM" ~ "Latin America",
    REMF == "CIS" ~ "CIS",
    REMF == "XME" ~ "Middle East",
    REMF == "XNF" ~ "North Africa",
    REMF == "XAF" ~ "Sub-Saharan Africa",
    REMF == "XOC" ~ "Oceania"
  ))%>%
  group_by(VEMF,Sr,Scenario,YEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus))

years  <-
    seq(min(df_CDR_cum$YEMF), max(df_CDR_cum$YEMF), by = 1)

df_full <- df_CDR_cum %>%
  group_by(VEMF,REMF,Scenario) %>%
  complete(YEMF = years) %>%
  arrange(VEMF,REMF,YEMF)

# ③ 線形補完
df_CDR_cum2 <- df_full %>%
  group_by(VEMF,REMF,Scenario) %>%
  mutate(IAMC_templatePlus = zoo::na.approx(IAMC_templatePlus, YEMF, na.rm = FALSE)) %>%
  mutate(IAMC_templatePlus = if_else(is.na(IAMC_templatePlus), 0, IAMC_templatePlus)) %>% 
  ungroup() %>% 
  group_by(VEMF,REMF,Scenario) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus))

df_CDR_cum2_sum <- df_CDR_cum2 %>%
  #filter(VEMF %in% c("植林")) %>% 
  group_by(Scenario,REMF) %>% 
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus))

g11_cum2 <- df_CDR_cum2 %>%
  #mutate(Sr = factor(Sr, levels = c("Europe","CIS","Middle East","India","China","Rest of Asia","North America",
  #                                  "Brazil","Latin America","North Africa","Sub-Saharan Africa","Oceania"))) %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=VEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",position="dodge",width = 0.8,color="black",size = 0.15)+
  labs(title = "",tag = "", x="", y = expression(paste('2030-2100 Reduction potential (Gt',{CO[2]},'eq)',seq="")))+
  facet_wrap(~REMF, nrow = 2)+
  theme_bw()+
  scale_fill_manual(values = color_bar1) +
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 14.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 15.25,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=16.75),
        strip.text.x = element_text(size = 14.5),legend.text = element_text(size =16.75),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),plot.tag =element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g11_cum2)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/CDR_cum_R5.png", plot=g11_cum2, width = 10.75, height = 6.85)

df_set_CDR_cum <- df_set %>%
  filter(VEMF %in% c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離")) %>%
  group_by(Scenario,REMF) %>%
  summarise(
    IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE)
  ) %>%
  mutate(VEMF = "CDR")

df_CDR_top50cum <- df_set_CDR_cum %>%
  filter(!(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))) %>% 
  group_by(Scenario,VEMF) %>%
  arrange(desc(IAMC_templatePlus)) %>%
  head(50)

df_CDR_top50cum <- df_set %>%
  filter(VEMF %in% c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))%>%
  group_by(Scenario,VEMF,REMF) %>%
  summarise(
    IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE)
  ) %>% 
  filter((REMF %in% c("RUS","CHN","CAN","BRA","USA","ARG","IND","AUS","KAZ","COD",
                      "MEX","DZA","IRN","ETH","TCD","IDN","PER","MNG","COL","ZAF",
                      "LBY","TUR","UKR","BOL","VEN","CAF","CMR","AFG","MMR","NAM",
                      "FRA","CIV","KEN","MDG","MAR","DEU","IRQ","AGO","PAK","THA",
                      "POL","GIN","UZB","ZWE","PHL","ESP","ITA","SAU","BWA","COG"))) %>%
  mutate(REMF = recode(REMF,
                       RUS = "Russia",
                       CHN = "China",
                       CAN = "Canada",
                       BRA = "Brazil",
                       USA = "United States",
                       ARG = "Argentina",
                       IND = "India",
                       AUS = "Australia",
                       KAZ = "Kazakhstan",
                       COD = "Congo, DR",
                       MEX = "Mexico",
                       DZA = "Algeria",
                       IRN = "Iran",
                       ETH = "Ethiopia",
                       TCD = "Chad",
                       IDN = "Indonesia",
                       PER = "Peru",
                       MNG = "Mongolia",
                       COL = "Colombia",
                       ZAF = "South Africa",
                       LBY = "Libya",
                       TUR = "Turkey",
                       UKR = "Ukraine",
                       BOL = "Bolivia",
                       VEN = "Venezuela",
                       CAF = "CAR",
                       CMR = "Cameroon",
                       AFG = "Afghanistan",
                       MMR = "Myanmar",
                       NAM = "Namibia",
                       FRA = "France",
                       CIV = "Côte d'Ivoire",
                       KEN = "Kenya",
                       MDG = "Madagascar",
                       MAR = "Morocco",
                       DEU = "Germany",
                       IRQ = "Iraq",
                       AGO = "Angola",
                       PAK = "Pakistan",
                       THA = "Thailand",
                       POL = "Poland",
                       GIN = "Guinea",
                       UZB = "Uzbekistan",
                       ZWE = "Zimbabwe",
                       PHL = "Philippines",
                       ESP = "Spain",
                       ITA = "Italy",
                       SAU = "Saudi Arabia",
                       BWA = "Botswana",
                       COG = "Republic of the Congo"
  ))

total_cum <- df_CDR_top50cum %>%
  group_by(REMF) %>%
  summarise(total_value = sum(IAMC_templatePlus))
total_cum <- total_cum %>%
  mutate(x = 0, y = 0)

g11_cum <- df_CDR_top50cum %>%
  mutate(VEMF = factor(VEMF, levels = c("植林","アグロフォレストリー",
                                        "バイオ炭","土壌炭素隔離"))) %>%
  mutate(REMF = factor(REMF, levels = c("Russia","China","Canada","Brazil","United States",
                                        "Argentina","India","Australia","Kazakhstan","Congo, DR",
                                        "Mexico","Algeria","Iran","Ethiopia","Chad",
                                        "Indonesia","Peru","Mongolia","Colombia","South Africa",
                                        "Libya","Turkey","Ukraine","Bolivia","Venezuela",
                                        "CAR","Cameroon","Afghanistan","Myanmar","Namibia",
                                        "France","Côte d'Ivoire","Kenya","Madagascar","Morocco",
                                        "Germany","Iraq","Angola","Pakistan","Thailand",
                                        "Poland","Guinea","Uzbekistan","Zimbabwe","Philippines",
                                        "Spain","Italy","Saudi Arabia","Botswana","Republic of the Congo"))) %>%
  ggplot() +
  geom_bar(aes(x="",y=-IAMC_templatePlus,fill=VEMF),stat = "identity",color="black",linewidth = 0.15)+
  #geom_text(aes(x = "", y =-Value/1000, label = label),
  #          position = position_stack(vjust = 0.5),inherit.aes = TRUE,size = 5) +
  geom_text(data = total_cum,
            aes(x = x, y = y, label = paste0(round(total_value, 1))),
            inherit.aes = FALSE,
            size = 5.00)+
  labs(title = "",x="", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  #facet_grid(rows = vars(cate), cols = vars(applicability), scales = "free_y")+
  facet_wrap(~REMF, ncol  = 10, scales = "free_y")+
  coord_polar("y") +                        # 極座標で変換（y軸を使う）
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_text(size = 20.75,color="black"), 
        axis.title.x = element_text(size = 20.75,color="black"), axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size = 15.00),strip.text.y = element_text(size = 15.00),legend.text = element_text(size =20.75),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),axis.ticks = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  #guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g11_cum)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/CDR削減内訳_cum.png", plot=g11_cum, width = 15.85, height = 10.25, units="in", dpi=300)


#一人当たり削減強度(各国レベル)-------------------------------------------------------------
world_data_pop <- world_data_2100[!is.na(world_data_2100$pop_est), ]
world_data_pop<- world_data_pop %>% filter(!is.na(Value))
world_data_pop <- world_data_pop %>%
  select(iso_a3_eh,pop_est,Value) %>% 
  filter(pop_est != 0)
  group_by(iso_a3_eh) %>%
  summarize(Value = sum((Value)*1000000)/sum((pop_est)))

world_na_pop<- world_data_pop %>% filter(is.na(Value))
world_with_data_pop <- world_data_pop %>% filter(!is.na(Value))

world_with_data_pop <- st_wrap_dateline(world_with_data_pop, options = c("WRAPDATELINE=YES"), quiet = TRUE)
world_na_pop <- st_wrap_dateline(world_na_pop, options = c("WRAPDATELINE=YES"), quiet = TRUE)

map_data_r17 <- read.table("../../data/Analysis_lu/region_iso_17.map", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
world2 <- left_join(map_data_r17, world, by = c("X.ISO" = "iso_a3_eh"))
world2 <- subset(world2, select = -X)
world2 <- subset(world2, select = -X.asterisks.for.map)
world3 <- world2 %>%
  mutate(REMF = case_when(
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

world3 <- subset(world3, select = c(X.ISO,REMF,AIM17,pop_est))
world3 <- world3[!is.na(world3$REMF), ]
world3 <- world3[!is.na(world3$pop_est), ]
world_data_pop_R5 <- world3 %>%
  group_by(REMF) %>%
  summarize(POP_agg = sum(pop_est)) %>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU"))

df_category_R5_sum <-df_red_cate_R5_2100 %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    Value = sum(IAMC_templatePlus)
  )

df_red_cate_R5_2100_pop <- world_data_pop_R5 %>%
  full_join(df_category_R5_sum, by = c("REMF"))%>%
  group_by(REMF, cate) %>%
  summarize(Potential = sum((Value)*1000000)/sum((POP_agg)))

g12<-
  ggplot() +
  #geom_sf(aes(fill = AFOLU/1000)) +
  geom_sf(data = world_with_data_pop, aes(fill = Value), color = "black") +
  geom_sf(data = world_na_pop, fill = "white", color = "black") +
  #scale_fill_viridis_c(name = "Mitigation potensial (MTCO2eq/yr)", option = "A") +
  scale_fill_gradient(name = expression(paste('Potential (t',{CO[2]},'eq/cap/yr)',seq="")),low = "#fff9f3", high = "#d94801") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag = "(c)")+
  guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =11.25),
        legend.position = c(0.10, 0.225),legend.title = element_text(size =12.75,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank(),plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
#guides(fill = guide_legend(nrow = 2))
plot(g12)

ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/PPT/2100_potential_pop.png", plot=g12, width = 10.15, height = 4.6)

color_bar <- c("LULUCF(CDR)"="#A5D6A7","LULUCF(その他)"="#ADD8E6","農業"="#FFCA28")

g13 <- df_red_cate_R5_2100_pop %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=Potential,fill=cate),stat = "identity")+
  #facet_wrap(~REMF, nrow = 5, scales = "free_y")+
  labs(title = "",  tag="(d)",x="", y = expression(paste('Mitigation potential (t',{CO[2]},'eq/cap/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar,
                    labels = c(
                      
                      "LULUCF\n(CDR)",         # 改行：LULUCF (CDR)
                      "LULUCF\n(その他)",      # 改行：LULUCF (その他)
                      "農業"
                    ))+
  theme(plot.title = element_text(size = 14.5), 
        axis.text.x = element_text(size = 11.5,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 12.5,color="black"), 
        axis.title.x = element_text(size=15), axis.title.y = element_text(size=12.75),
        strip.text.x = element_text(size = 25),legend.text = element_text(size =11.50),
        legend.position = "right",legend.title = element_blank(),
        panel.border = element_blank(),  # 全体の囲い線を非表示
        #panel.grid.major.x = element_line(),  # 左側のグリッド線を表示
        #panel.grid.major.y = element_line() ,
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),plot.margin = margin(t = 10, b = 10))
#guides(fill = guide_legend(nrow = 2))
plot(g13)

g14 <-(g12 | g13) + plot_layout(widths = c(5, 1), heights = c(1, 1))
plot(g14)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/2100_potential_pop.png", plot=g14, width = 11.15, height = 4.6)

g15 <-(g4_1 / g14) + plot_layout(widths = c(1, 1), heights = c(1, 1))
plot(g15)
ggsave(file = "../../output/thesis/4.4 AFOLU分野の削減ポテンシャル/2100_potential_all.png", plot=g15, width = 12.15, height = 9.75)
