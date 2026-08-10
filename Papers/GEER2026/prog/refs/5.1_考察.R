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

#SET_更新後--------------------------------------------------------------------
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
                     Emi_Red_CO2_AFO_Lan_Pla="植林",Emi_Red_CO2_AFO_Lan_Ref="再植林",Emi_Red_CO2_AFO_Lan_FPr="森林保護",Emi_Red_CO2_AFO_Lan_Pff="森林火災の防止","Emi_Red_CO2_AFO_Lan_Ril"="環境負荷の少ない伐採",Emi_Red_CO2_AFO_Lan_Enr="自然再生の強化",Emi_Red_CO2_AFO_Lan_Agf="アグロフォレストリー",Emi_Red_CO2_AFO_Lan_Bch="バイオ炭",Emi_Red_CO2_AFO_Lan_Scs="土壌炭素隔離"))%>%
  rename(new = IAMC_templatePlus)
df_set <- df_set %>%
  mutate(new = ifelse(new < 0, -new, new))

#SET_更新前--------------------------------------------------------------------
df_set_old_agr_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_a.gdx", "IAMC_templatePlus")
df_set_old_agr_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_b.gdx", "IAMC_templatePlus")
df_set_old_agr <- full_join(df_set_old_agr_a, df_set_old_agr_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_old_agr <- df_set_old_agr %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_old_agr <- df_set_old_agr %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin"))

df_set_old_lu <- rgdx.param("../../data/final_report/IAMC_templatePlus_unupdate.gdx", "IAMC_templatePlus")
df_set_old_lu <- df_set_old_lu %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))


df_set_old <- full_join(df_set_old_agr, df_set_old_lu, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))

df_set_old <- df_set_old %>%
  #filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
  #                   "Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))%>%
  mutate(VEMF=recode(VEMF,Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre="生産性が高い家畜種への変更",Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee="濃厚飼料の投与",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad="ラグーンカバー",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And="嫌気性消化(大規模プラント)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl="嫌気性消化(小規模)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc="糞尿堆肥化",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams="窒素肥料の硫酸アンモニウムへの変更",Emi_Red_equ_CH4_AFO_Agr_Ric_Wat="水田への間断灌漑の導入",Emi_Red_equ_CH4_AFO_Agr_Ric_Res="稲わらのすき込み",
                     Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til="不耕起移植栽培",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer="肥料の高効率利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res="化学肥料の有機肥料による置き換え",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo="被覆肥料の利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin="硝化抑制剤を含む肥料の利用",
                     Emi_Red_CO2_AFO_Lan_Pla="植林",Emi_Red_CO2_AFO_Lan_Ref="再植林",Emi_Red_CO2_AFO_Lan_FPr="森林保護",Emi_Red_CO2_AFO_Lan_Pff="森林火災の防止","Emi_Red_CO2_AFO_Lan_Ril"="環境負荷の少ない伐採",Emi_Red_CO2_AFO_Lan_Enr="自然再生の強化",Emi_Red_CO2_AFO_Lan_Agf="アグロフォレストリー",Emi_Red_CO2_AFO_Lan_Bch="バイオ炭",Emi_Red_CO2_AFO_Lan_Scs="土壌炭素隔離"))%>%
  rename(old = IAMC_templatePlus)
df_set_old <- df_set_old %>%
  mutate(old = ifelse(old < 0, -old, old))

#merge-------------------------------------------------------------------------
df_red_land <- left_join(df_set_old, df_set, by = c("REMF", "Scenario", "YEMF","VEMF"))%>%
  filter(Scenario %in% c("SSP2_CM_NoCC"))%>%
  pivot_longer(cols=c("old","new"),names_to="Run",values_to="Value")
df_red_land <- df_red_land %>% 
  filter(!is.na(Value))

#変化量_GL-----------------------------------------------------------------------
df_red_land_sub <- df_red_land %>%
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

df_red_land_sub_sum <- df_red_land_sub %>%
  group_by(REMF,YEMF,sub,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_red_land_sub_GL <- filter(df_red_land_sub_sum, REMF %in% c("World"))

df_red_cate <- df_red_land %>%
  mutate(cate = case_when(
    VEMF == "植林" ~ "LULUCF",
    VEMF == "再植林" ~ "LULUCF",
    VEMF == "森林保護" ~ "LULUCF",
    VEMF == "環境負荷の少ない伐採" ~ "LULUCF",
    VEMF == "自然再生の強化" ~ "LULUCF",
    VEMF == "森林火災の防止" ~ "LULUCF",
    VEMF == "アグロフォレストリー" ~ "LULUCF",
    VEMF == "バイオ炭" ~ "LULUCF",
    VEMF == "土壌炭素隔離" ~ "LULUCF",
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

df_red_cate_sum <- df_red_cate %>%
  group_by(REMF,YEMF,cate,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_red_cate_GL <- df_red_cate_sum %>% 
  filter(REMF %in% c("World")) %>% 
  filter(YEMF %in% c("2030","2050","2100")) %>% 
  mutate(Run=recode(Run,new="更新後",old="更新前"))

df_red_all <- df_red_cate_GL %>%
  group_by(REMF,YEMF,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_red_cate_GL_ch <- df_red_cate_GL %>%
  pivot_wider(names_from = Run, values_from = Value)%>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    Value = sum(new)-sum(old)
  )

color_bar <- c("LULUCF"="#A5D6A7","農業"="#F4B400")

g1 <- df_red_cate_GL %>%
  #mutate(REMF = factor(REMF, levels = c("World","OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(cate = factor(cate, levels = c("農業", "LULUCF"))) %>%
  ggplot() +
  geom_bar(aes(x=Run,y=Value/1000,fill=cate),stat = "identity",width = 0.8,position = position_stack(reverse = TRUE),color="black",size = 0.15)+
  labs(title = "",tag = "", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~YEMF, nrow = 1)+
  theme_bw()+
  scale_fill_manual(values = color_bar) +
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 14.05,color="black"), axis.text.y = element_text(size = 14.05,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=15.50),
        strip.text.x = element_text(size = 14.05),legend.text = element_text(size =15.5),
        legend.position = "right",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),plot.tag =element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g1)

ggsave(file = "../../output/thesis/5.1 考察および結論/変化量_世界.png", plot=g1, width = 12.75, height = 4.25)

ggsave(file = "../../output/thesis/5.1 考察および結論/変化量_世界_PPT.png", plot=g1, width = 11.25, height = 5.25)
