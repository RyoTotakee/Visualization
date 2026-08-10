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

#SET--------------------------------------------------------------------
df_set_limit_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_a.gdx", "IAMC_templatePlus")
df_set_limit_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_b.gdx", "IAMC_templatePlus")
df_set_limit <- full_join(df_set_limit_a, df_set_limit_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_limit <- df_set_limit %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_limit <- df_set_limit %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  rename(Limit = IAMC_templatePlus) 

df_set_nolimit_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_a.gdx", "IAMC_templatePlus")
df_set_nolimit_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_b.gdx", "IAMC_templatePlus")
df_set_nolimit <- full_join(df_set_nolimit_a, df_set_nolimit_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_nolimit <- df_set_nolimit %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_nolimit <- df_set_nolimit %>%
  filter(Scenario %in% c("SSP2_CM_NoCC"))%>%
  #filter(REMF %in% c("CHN","CAN","USA"))%>%
  rename(NoLimit = IAMC_templatePlus)

df_limit_nolimit <- full_join(df_nolimit, df_limit, by = c("REMF", "Scenario", "YEMF","VEMF"))
df_limit_nolimit <- df_limit_nolimit %>% 
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl",
                     "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer",
                     "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin"))%>%
  mutate(VEMF=recode(VEMF,Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre="生産性が高い家畜種への変更",Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee="濃厚飼料の投与",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad="ラグーンカバー",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And="嫌気性消化(大規模プラント)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl="嫌気性消化(小規模)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc="糞尿堆肥化",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams="窒素肥料の硫酸アンモニウムへの変更",Emi_Red_equ_CH4_AFO_Agr_Ric_Wat="水田への間断灌漑の導入",Emi_Red_equ_CH4_AFO_Agr_Ric_Res="稲わらのすき込み",
                     Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til="不耕起移植栽培",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer="肥料の高効率利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res="化学肥料の有機肥料による置き換え",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo="被覆肥料の利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin="硝化抑制剤を含む肥料の利用",
  ))
df_limit_nolimit <- df_limit_nolimit %>% 
  pivot_longer(cols=c("Limit","NoLimit"),names_to="Run",values_to="Value")
df_limit_nolimit <- df_limit_nolimit %>%
  mutate(Value = ifelse(Value < 0, -Value, Value))
df_limit_nolimit[is.na(df_limit_nolimit)] <- 0

#変化量_世界全体--------------------------------------------------------------------
df_limit_nolimit_gl <- df_limit_nolimit %>% 
  filter((YEMF %in% c("2030","2050","2100"))) %>% 
  filter((REMF %in% c("World")))

df_limit_nolimit_gl_sub <- df_limit_nolimit_gl %>%
  mutate(sub = case_when(
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

df_limit_nolimit_gl_sub_sum <- df_limit_nolimit_gl_sub %>%
  group_by(REMF,YEMF,sub,Run) %>%
  summarize(
    Value = sum(Value)
  )
df_sum_GL <- df_limit_nolimit_gl_sub_sum %>%
  group_by(REMF,YEMF,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_limit_nolimit_dff <- df_limit_nolimit_gl_sub_sum %>%
  pivot_wider(names_from = Run, values_from = Value)%>%
  group_by(REMF,YEMF,sub) %>%
  summarize(
    Value = sum(Limit)-sum(NoLimit)
  )


df_dff_GL <- df_limit_nolimit_dff %>% 
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(Value)
  )
df_limit_nolimit_dff <- df_limit_nolimit_dff %>%
  filter(Value != 0)

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8"
)

g1 <- df_limit_nolimit_dff %>%
  mutate(REMF = factor(REMF, levels = c("World"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=Value/1000,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(tag = "(a)",x="Year", y = expression(paste('Change in reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  #facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 18.25,color="black"), axis.text.y = element_text(size = 18.25,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=22.50),
        strip.text.x = element_text(size = 22.5),strip.text.y = element_text(size = 22.5),legend.text = element_text(size =20.15),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 2))+
  xlab(NULL)
plot(g1)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/Limit_gl_dff.png", plot=g1, width = 7.75, height = 8.50)

#Limit-----------------------------------------------------------------------------------------------
df_limit_nolimit_gl <- df_limit_nolimit %>% 
  filter((YEMF %in% c("2030","2050","2100"))) %>% 
  filter((REMF %in% c("World")))

df_limit_nolimit_cate <- df_limit_nolimit_gl %>%
  mutate(cate = case_when(
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

df_dff_cate <- df_limit_nolimit_cate %>%
  pivot_wider(names_from = Run, values_from = Value) %>% 
  group_by(REMF,YEMF,cate,VEMF) %>%
  summarize(
    Value = sum(Limit)-sum(NoLimit)
  )
df_dff_cate <- df_dff_cate %>%
  filter(Value != 0)

df_dff_cate_sum <- df_dff_cate %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(
    Value = sum(Value)
  )%>%
  mutate(cate = factor(cate, 
                       levels = c("反芻対策", "糞尿対策", "土壌対策")))

color_bar1 <- c(
  "生産性が高い家畜種への変更"        = "#fc8d62",  # orange
  "濃厚飼料の投与"                  = "#56B4E9",  # sky blue
  "ラグーンカバー"                  = "#009E73",  # green
  "糞尿堆肥化"                      = "#e78ac3",  # yellow
  "嫌気性消化(大規模プラント)"      = "#0072B2",  # blue
  "嫌気性消化(小規模)"              = "#D55E00",  # reddish orange
  "窒素肥料の硫酸アンモニウムへの変更" = "#80b1d3",  # pinkish purple
  "水田への間断灌漑の導入"          = "#b3de69",  # grey
  "稲わらのすき込み"                = "#A6761D",  # brown
  "不耕起移植栽培"                  = "#ffd92f",  # turquoise
  "肥料の高効率利用"                = "#984ea3",  # coral
  "化学肥料の有機肥料による置き換え" = "#e5c494",  # light blue
  "被覆肥料の利用"                  = "#66C2A5",  # pink
  "硝化抑制剤を含む肥料の利用"      = "#377eb2"   # greenish yellow
)

g2 <- df_dff_cate %>%
  #mutate(VEMF = factor(VEMF, levels = c("生産性が高い家畜種への変更","濃厚飼料の投与",
  #"ラグーンカバー","嫌気性消化(大規模プラント)","嫌気性消化(小規模)","糞尿堆肥化", 
  # "窒素肥料の硫酸アンモニウムへの変更", "水田への間断灌漑の導入", "稲わらのすき込み",
  #  "不耕起移植栽培","肥料の高効率利用","化学肥料の有機肥料による置き換え", "被覆肥料の利用","硝化抑制剤を含む肥料の利用"))) %>%
  mutate(cate = factor(cate, levels = c("反芻対策", "糞尿対策",  "土壌対策"))) %>%
  #mutate(applicability = factor(applicability, levels = c("nonlim", "lim_var", "lim_const"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=Value/1000,fill=VEMF),stat = "identity",color="black",size=0.15)+
  geom_hline(yintercept = 0, color = "grey15", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_point(data = df_dff_cate_sum, aes(x=YEMF,y=Value/1000,color=1,group=1), size = 2,color="black")+
  labs(tag = "(b)",x="Year", y =expression(paste('Change in reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  #facet_grid(rows = vars(cate), cols = vars(applicability), scales = "free_y")+
  facet_wrap(~cate, ncol = 2, scales = "free_y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 18.25,color="black"), axis.text.y = element_text(size = 18.50,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=22.50),
        strip.text.x = element_text(size = 18.5),strip.text.y = element_text(size = 20.5),legend.text = element_text(size =20.15,margin = margin(t = 6.25, b = 6.25)),
        legend.position = "right",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), legend.spacing.y = unit(1.5, "cm"),
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white")
        #,legend.justification = c(0, 1)
  )+
  #guides(fill = guide_legend(nrow = 3))+
  xlab(NULL)
plot(g2)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/Mtigation_cate_dff.png", plot=g2, width = 15.75, height = 8.5)

g3 <-(g1 | g2) + plot_layout(widths = c(0.7, 1))
plot(g3)
ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/dff.png", plot=g3, width = 16.75, height = 8.00)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/dff_PPT.png", plot=g3, width = 16.75, height = 10.00)

#変化量_地域・国--------------------------------------------------------------------
df_lim_nolim_R5 <- df_limit_nolimit %>% 
  filter((YEMF %in% c("2030","2050","2100")))  %>% 
  filter((REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))) %>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU"))

df_lim_nolim_R5_sub <- df_lim_nolim_R5 %>%
  mutate(sub = case_when(
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

df_lim_nolim_R5_sub_sum <- df_lim_nolim_R5_sub %>%
  group_by(REMF,YEMF,sub,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_lim_nolim_R5_sum <- df_lim_nolim_R5_sub %>%
  group_by(REMF,YEMF,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_rate_R5 <- df_lim_nolim_R5_sub %>%
  pivot_wider(names_from = Run, values_from = Value)%>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = ((sum(Limit)-sum(NoLimit))/sum(NoLimit))
  ) %>% 
  filter(YEMF%in% c("2030"))
df_rate_R5 <- df_rate_R5 %>%
  filter(Value != 0)

df_dff_R5 <- df_lim_nolim_R5_sub_sum %>%
  pivot_wider(names_from = Run, values_from = Value)%>%
  group_by(REMF,YEMF,sub) %>%
  summarize(
    Value = sum(Limit)-sum(NoLimit)
  )
df_dff_R5 <- df_dff_R5 %>%
  filter(Value != 0)

df_dff_R5_ch <- df_dff_R5 %>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(Value)
  )

df_lim_nolim_case <- df_limit_nolimit %>% 
  filter((YEMF %in% c("2030","2050","2100")))  %>% 
  filter((REMF %in% c("IND","CHN","TUR","ETH","NGA","AFG")))
df_lim_nolim_case$Value[is.na(df_lim_nolim_case$Value)] <- 0

df_lim_nolim_case_cate <- df_lim_nolim_case %>%
  mutate(cate = case_when(
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

df_lim_nolim_case_cate_sum <- df_lim_nolim_case_cate %>%
  group_by(REMF,YEMF,cate,Run,VEMF) %>%
  summarize(
    Value = sum(Value)
  )

df_dff_case <- df_lim_nolim_case_cate_sum %>%
  pivot_wider(names_from = Run, values_from = Value)%>%
  group_by(REMF,YEMF,cate,VEMF) %>%
  summarize(
    Value = sum(Limit)-sum(NoLimit)
  )

color_bar1_1 <- c(
  "反芻対策" = "#FFA040",
  "糞尿対策" = "#F48FB1",
  "稲作対策" = "#FFEE58",
  "土壌対策" = "#CE93D8"
)

g4 <- df_dff_R5 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=Value/1000,fill=sub),stat = "identity",color="black",size = 0.15)+
  labs(tag = "",x="Year", y = expression(paste('Change in reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),plot.tag=element_blank(),
        axis.text.x = element_text(size = 18.25,color="black"), axis.text.y = element_text(size = 18.25,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=22.50),
        strip.text.x = element_text(size = 22.5),strip.text.y = element_text(size = 22.5),legend.text = element_text(size =18.25),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g4)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/Limit_R5_dff.png", plot=g4, width = 12.75, height = 7.50)

g5 <- df_dff_case %>%
  mutate(VEMF = factor(VEMF, levels = c("生産性が高い家畜種への変更","濃厚飼料の投与",
                                        "ラグーンカバー","嫌気性消化(大規模プラント)","嫌気性消化(小規模)","糞尿堆肥化", 
                                        "窒素肥料の硫酸アンモニウムへの変更", "水田への間断灌漑の導入", "稲わらのすき込み",
                                        "不耕起移植栽培","肥料の高効率利用","化学肥料の有機肥料による置き換え", "被覆肥料の利用","硝化抑制剤を含む肥料の利用"))) %>%
  mutate(REMF = factor(REMF, levels = c("IND", "CHN", "TUR", "ETH", "NGA", "AFG"))) %>%
  mutate(cate = factor(cate, levels = c("反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  #mutate(applicability = factor(applicability, levels = c("nonlim", "lim_var", "lim_const"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=Value,fill=VEMF),stat = "identity")+
  geom_hline(yintercept = 0, color = "grey15", linetype = "solid", size = 0.5, alpha = 0.7) +
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_grid(rows = vars(REMF), cols = vars(cate), scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 17.25,color="black"), axis.text.y = element_text(size = 16.25,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=24.25),
        strip.text.x = element_text(size = 22.5),strip.text.y = element_text(size = 22.5),legend.text = element_text(size =25.55),
        legend.position = "right",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  #guides(fill = guide_legend(nrow = 3))+
  xlab(NULL)
plot(g5)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/MitPot_case8_dff.png", plot=g5, width = 17.50, height = 10.25)

g6 <- df_lim_nolim_R5_sub %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(cate = factor(sub, levels = c("反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  #mutate(applicability = factor(applicability, levels = c("nonlim", "lim_var", "lim_const"))) %>%
  ggplot() +
  geom_bar(aes(x=Run,y=Value/1000,fill=sub),stat = "identity")+
  geom_hline(yintercept = 0, color = "grey15", linetype = "solid", size = 0.5, alpha = 0.7) +
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_grid(rows = vars(REMF), cols = vars(YEMF), scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1_1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(size = 17.25,color="black"), axis.text.y = element_text(size = 16.25,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=24.25),
        strip.text.x = element_text(size = 22.5),strip.text.y = element_text(size = 22.5),legend.text = element_text(size =25.55),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  #guides(fill = guide_legend(nrow = 3))+
  xlab(NULL)
plot(g6)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/MitPot_R5.png", plot=g6, width = 17.00, height = 11.85)

g7 <- df_rate_R5 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  #mutate(sub = factor(sub, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=REMF,y=Value),stat = "identity",color="black",size = 0.15)+
  geom_hline(yintercept = 0, color = "grey15", linetype = "solid", size = 0.25, alpha = 0.7) +
  labs(tag = "",x="Year", y = "Percent Change from BaU (%)")+
  #facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  #scale_fill_manual(values = color_bar1_1)+
  scale_y_continuous(
                      labels = percent_format(accuracy = 1)) +
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  theme(plot.title = element_blank(),plot.tag=element_blank(),
        axis.text.x = element_text(size = 18.25,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18.25,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=22.50),
        strip.text.x = element_text(size = 22.5),strip.text.y = element_text(size = 22.5),legend.text = element_text(size =18.25),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g7)

ggsave(file = "../../output/thesis/4.2 農業分野の技術制約/Limit_R5_rate.png", plot=g7, width = 5.75, height = 8.25)
