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
#Mitigation_potential
df_set_lu_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_a.gdx", "IAMC_templatePlus")
df_set_lu_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_b.gdx", "IAMC_templatePlus")
df_set_lu <- full_join(df_set_lu_a, df_set_lu_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_lu <- df_set_lu %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_new <- df_set_lu %>%
  filter(Scenario %in% c("SSP2_CM_NoCC"))%>%
  #filter(REMF %in% c("CHN","CAN","USA"))%>%
  rename(new = IAMC_templatePlus)

df_old <- rgdx.param("../../data/final_report/IAMC_templatePlus_unupdate.gdx", "IAMC_templatePlus")
df_old <- df_old %>%
  filter(Scenario %in% c("SSP2_CM_NoCC"))%>%
  #filter(REMF %in% c("CHN","CAN","USA"))%>%
  rename(old = IAMC_templatePlus)

df_red_land <- left_join(df_old, df_new, by = c("REMF", "Scenario", "YEMF","VEMF"))%>%
  filter(Scenario %in% c("SSP2_CM_NoCC"))%>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))%>%
  mutate(VEMF=recode(VEMF,Emi_Red_CO2_AFO_Lan_Pla="Plantation",Emi_Red_CO2_AFO_Lan_Ref="Reforestation",Emi_Red_CO2_AFO_Lan_FPr="Forest protection",
                     Emi_Red_CO2_AFO_Lan_Pff="Prevention of forest fire",Emi_Red_CO2_AFO_Lan_Ril="Reduced Impact logging",Emi_Red_CO2_AFO_Lan_Enr="Enhanced natural regeneration",
                     Emi_Red_CO2_AFO_Lan_Agf="Agroforestry",Emi_Red_CO2_AFO_Lan_Bch="Biochar",Emi_Red_CO2_AFO_Lan_Scs="Soil carbon sequestration"))%>% 
  pivot_longer(cols=c("old","new"),names_to="Run",values_to="Value")

df_red_land <- df_red_land %>%
  mutate(Value = ifelse(Value < 0, -Value, Value))

#Land_area
df_area_land <- left_join(df_old, df_new, by = c("REMF", "Scenario", "YEMF","VEMF"))
df_area_Scs <- df_area_land %>%
  filter(VEMF %in% c("Lan_Are_Scg", "Lan_Are_Scc")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarise(
    new = sum(new, na.rm = TRUE),
    old = sum(old, na.rm = TRUE)
  ) %>%
  mutate(VEMF = "Lan_Are_Scs") 
df_area_land <- bind_rows(
  df_area_land,
  df_area_Scs)

df_area_land <- df_area_land %>%
  filter(VEMF %in% c("Lan_Are_Pla","Lan_Are_Ref","Lan_Are_FPr","Lan_Are_Pff","Lan_Are_Ril","Lan_Are_Enr","Lan_Are_Agf","Lan_Are_Bch","Lan_Are_Scs"))%>%
  mutate(VEMF=recode(VEMF,Lan_Are_Pla="Plantation",Lan_Are_Ref="Reforestation",Lan_Are_FPr="Forest protection",
                     Lan_Are_Pff="Prevention of forest fire",Lan_Are_Ril="Reduced Impact logging",Lan_Are_Enr="Enhanced natural regeneration",
                     Lan_Are_Agf="Agroforestry",Lan_Are_Bch="Biochar",Lan_Are_Scs="Soil carbon sequestration"))
df_area_land <- df_area_land %>% 
  pivot_longer(cols=c("old","new"),names_to="Run",values_to="Value")
df_area_land <- df_area_land %>%
  mutate(Value = ifelse(Value < 0, -Value, Value))

df_AR6 <- read_excel("../../data/plantation/AR6_afr.xlsx")
df_AR6_land <- read_excel("../../data/plantation/AR6_LAND.xlsx")

#Afr_Global--------------------------------------------------------------------
df_red_Afr_Gl <- df_red_land %>%
  filter(REMF %in% c("World"))%>%
  filter(VEMF %in% c("Plantation"))%>%
  filter(Run %in% c("new"))

df_area_Afr_Gl <- df_area_land %>%
  filter(REMF %in% c("World"))%>%
  filter(VEMF %in% c("Plantation"))%>%
  filter(Run %in% c("new"))

df_AR6 <- df_AR6 %>%
  pivot_longer(cols=c("2000","2005","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100"),names_to="Year",values_to="Value")
df_AR6 <- na.omit(df_AR6)%>%
  filter(Year %in% c("2030","2040","2050","2060","2070","2080","2090","2100"))

df_AR6_land <- df_AR6_land %>%
  pivot_longer(cols=c("2000","2005","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100"),names_to="Year",values_to="Value")
df_AR6_land <- na.omit(df_AR6_land)%>%
  filter(Year %in% c("2030","2040","2050","2060","2070","2080","2090","2100"))

summary_ar6 <- df_AR6 %>%
  #filter(grepl("Afforestation", Variable)) %>%
  group_by(Year) %>%
  summarise(
    min_value = min(Value, na.rm = TRUE),
    max_value = max(Value, na.rm = TRUE),
    median_value = median(Value, na.rm = TRUE)
  )

summary_custom <- df_AR6 %>% 
  #group_by(Year) %>% 
  #reframe(Value=sum(Value)) %>% 
  group_by(Year) %>% 
  reframe(p50=median(Value),p90=quantile(Value,.95),p10=quantile(Value,.05),p0=min(Value),p100=max(Value),n=n(),
          #Variable='Hydrogen\n(total)'
  )

summary_ar6_land <- df_AR6_land %>%
  #filter(grepl("Afforestation", Variable)) %>%
  group_by(Year) %>%
  summarise(
    min_value = min(Value, na.rm = TRUE),
    max_value = max(Value, na.rm = TRUE),
    median_value = median(Value, na.rm = TRUE)
  )


summary_custom_land <- df_AR6_land %>% 
  #group_by(Year) %>% 
  #reframe(Value=sum(Value)) %>% 
  group_by(Year) %>% 
  reframe(p50=median(Value),p90=quantile(Value,.95),p10=quantile(Value,.05),p0=min(Value),p100=max(Value),n=n(),
          #Variable='Hydrogen\n(total)'
  )

g1<-
  #mutate(VEMF = factor(VEMF, levels = c("Plantation", "Reforestation", "Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration", "Agroforestry", "Biochar", "Soil carbon sequestration"))) %>%
  ggplot() +
  #geom_ribbon(data = summary_ar6,
  #            aes(x = Year, ymin = min_value/1000, ymax = max_value/1000, fill = "AR6報告値",group = 1))+
  geom_crossbar(data=summary_custom %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p10/1000,ymax=p90/1000,y=p50/1000),width=.5,color='white',fill='grey80',linewidth=0.3)+
  geom_crossbar(data=summary_custom %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p0/1000,ymax=p100/1000,y=p0/1000),width=.5,color='grey80',fill='transparent',fatten=0,linewidth=0.3)+
  #geom_line(data = summary_ar6, aes(x = Year, y = median_value/1000,color = "AR6報告値の中央値", linetype = "AR6報告値の中央値", group = 1)) +
  #
  geom_line(data = df_red_Afr_Gl,aes(x = YEMF, y = Value/1000, color="本研究", linetype = "本研究", group=1),size = 1) +
  geom_point(data = df_red_Afr_Gl, aes(x=YEMF,y=Value/1000,color="本研究", group=1),size = 2.5)+
  labs(tag="(a)",title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  #facet_wrap(~REMF, nrow = 4, scales = "free_y")+
  theme_bw()+
  #scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  # 凡例の名前を設定
  scale_color_manual(name = "Line Type",
                     values = c("AR6報告値の中央値" = "darkgrey", "本研究" = "red")) +
  scale_linetype_manual(name = "Line Type",
                        values = c("AR6報告値の中央値" = "dashed", "本研究" = "solid")) +
  #scale_fill_manual(name = "Range",
  #                  values = c("AR6報告値" = "grey80")) +
  theme(plot.title = element_blank(),plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 15.25,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=17.25),
        strip.text.x = element_text(size = 15.5),legend.text = element_text(size =17),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g1)


ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/Afr_AR6_比較.png", plot=g1, width = 8, height = 6)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/PPT/Afr_AR6_比較.png", plot=g1, width = 8.5, height = 4.75)

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

g1_a<-
  #mutate(VEMF = factor(VEMF, levels = c("Plantation", "Reforestation", "Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration", "Agroforestry", "Biochar", "Soil carbon sequestration"))) %>%
  ggplot() +
  #geom_ribbon(data = summary_ar6_land,
  #            aes(x = Year, ymin = min_value/1000, ymax = max_value/1000, fill = "AR6報告値",group = 1))+
  #geom_line(data = summary_ar6_land, aes(x = Year, y = median_value/1000,color = "AR6報告値の中央値", linetype = "AR6報告値の中央値", group = 1)) +
  geom_crossbar(data=summary_custom_land %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p10/1000,ymax=p90/1000,y=p50/1000),width=.5,color='white',fill='grey80',linewidth=0.3)+
  geom_crossbar(data=summary_custom_land %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p0/1000,ymax=p100/1000,y=p0/1000),width=.5,color='grey80',fill='transparent',fatten=0,linewidth=0.3)+
  geom_line(data = df_area_Afr_Gl,aes(x = YEMF, y = Value/1000, color="本研究", linetype = "本研究", group=1),size = 1) +
  geom_point(data = df_area_Afr_Gl, aes(x=YEMF,y=Value/1000,color="本研究", group=1),size = 2.5)+
  labs(tag="(b)",title = "",x="Year", y = "Area of Afforestation (billion ha/yr)")+
  #facet_wrap(~REMF, nrow = 4, scales = "free_y")+
  theme_bw()+
  #scale_fill_manual(values = color_bar1)+
  #scale_y_continuous(limits = c(min_value, max_value),breaks = c(0, 2000, 4000, -2000),labels = c("0","2000", "4000", "-2000"))+
  #scale_y_continuous(limits = c(-max(abs(df_all_jap$EMFtemp)), max(abs(df_all_jap$EMFtemp)))) +
  # 凡例の名前を設定
  scale_color_manual(name = "Line Type",
                     values = c("AR6報告値の中央値" = "darkgrey", "本研究" = "red")) +
  scale_linetype_manual(name = "Line Type",
                        values = c("AR6報告値の中央値" = "dashed", "本研究" = "solid")) +
  scale_fill_manual(name = "Range",
                    values = c("AR6報告値" = "grey80")) +
  theme(plot.title = element_blank(),plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 15.25,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=10.75), axis.title.y = element_text(size=17.25),
        strip.text.x = element_text(size = 15.5),legend.text = element_text(size =17),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g1_a)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/Afr_AR6_比較_面積.png", plot=g1_a, width = 8, height = 6)

g1_b <-(g1 | g1_a |l_fig_1de) + plot_layout(widths = c(1, 1, 0.325))
plot(g1_b)
ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/AR6_all.png", plot=g1_b, width = 12.75, height = 6.00)

#CDR世界分布--------------------------------------------------------------------
df_CDR <- df_red_land %>%
  filter(VEMF %in% c("Plantation", "Agroforestry","Biochar","Soil carbon sequestration")) %>%
  group_by(Scenario,REMF,YEMF,Run) %>%
  summarise(
    Value = sum(Value, na.rm = TRUE)
  ) %>%
  mutate(VEMF = "CDR")
df_other <- df_red_land %>%
  filter(VEMF %in% c("Forest protection", "Prevention of forest fire", "Reduced Impact logging", "Enhanced natural regeneration")) %>%
  group_by(Scenario,REMF,YEMF,Run) %>%
  summarise(
    Value = sum(Value, na.rm = TRUE)
  ) %>%
  mutate(VEMF = "other")

df_red_CDR <- bind_rows(
  df_other,
  df_CDR)

df_CDR_R178 <- df_CDR %>% 
  filter(!(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))) %>% 
  filter((YEMF %in% c("2100")))
df_CDR_R178_new <- filter(df_CDR_R178,(Run %in% c("new")))
df_CDR_R178_old <- filter(df_CDR_R178,(Run %in% c("old")))
df_CDR_GL <- df_CDR %>% 
  filter((REMF %in% c("World"))) %>% 
  filter((YEMF %in% c("2100")))

df_CDR_MAF <- df_CDR %>% 
  filter((REMF %in% c("R5MAF"))) %>% 
  filter((YEMF %in% c("2100")))

df_median <- df_CDR_R178 %>%
  group_by(Scenario,YEMF,Run) %>%
  summarise(
    Value = median(Value)
  )


world_set <- ne_countries(scale = "medium", returnclass = "sf")
world_set2 <- world_set %>%
  filter(!name == "Antarctica")
world <- world_set2 %>%
  filter(name != "Greenland")

breaks <- quantile(df_CDR_R178_new$Value, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
breaks <- round(breaks, 0)
breaks <- unique(breaks) 

world_data <- world %>%
  left_join(df_CDR_R178_new, by = c("iso_a3_eh" = "REMF"))%>%
  mutate(
    cat = cut(
      Value,
      breaks = c(0, 2, 10, 32, 80, 140, 160),
      labels = c("0–2", "2–10", "10–32", "32–80", "80–140","140–"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

world_data_old <- world %>%
  left_join(df_CDR_R178_old, by = c("iso_a3_eh" = "REMF"))%>%
  mutate(
    cat = cut(
      Value,
      breaks = c(0, 2, 10, 32, 80, 140, 620),
      labels = c("0–2", "2–10", "10–32", "32–80", "80–140","140–"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

color_bar2 <- c(
  "0–2"     = "#f7fcf5",  # 非常に薄い緑（ほぼ白）
  "2–10"     = "#d0f0b2",  # 淡い黄緑
  "10–32"    = "#66bd63",  # 明るい緑
  "32–80"   = "#238443",  # 緑
  "80–140"  = "#006633",  # 濃い緑
  "140–" = "#00441b"   # 最も深い緑（最大値）
)

g2<- world_data %>% 
  ggplot() +
  geom_sf(aes(fill = cat),color = "grey20", size = 0.05) +
  #scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#f7fcf5", high = "#003300") +
  scale_fill_manual(
    name = "CDR\n(MtCO2eq/yr)",
    values = color_bar2,
    na.value = "lightgrey",
    drop = FALSE
  ) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag ="(a)")+
  #guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =11.25),
        legend.position = c(0.1, 0.325),legend.title = element_text(size =12.75,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank(),plot.margin = margin(t = 0, b = 0))
plot(g2)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_new.png", plot=g2, ,width = 8.75, height = 3.75)

g3<- world_data_old %>% 
  ggplot() +
  geom_sf(aes(fill = cat),color = "grey20", size = 0.05) +
  #scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#f7fcf5", high = "#003300") +
  scale_fill_manual(
    name = "CDR\n(MtCO2eq/yr)",
    values = color_bar2,
    na.value = "lightgrey",
    drop = FALSE
  ) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag ="(b)")+
  #guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =11.25),
        legend.position = c(0.1, 0.325),legend.title = element_text(size =12.75,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank(),plot.margin = margin(t = 0, b = 0))
plot(g3)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_old.png", plot=g3, ,width = 8.75, height = 3.75)

p_all <- (g2 / g3)
ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/Mtigation_CDR_2100.png", plot=p_all,width = 8.50, height = 7.25)

#CDR世界変化--------------------------------------------------------------------
df_CDR_gl <- df_red_land %>%
  filter(VEMF %in% c("Plantation", "Agroforestry","Biochar","Soil carbon sequestration")) %>% 
  mutate(VEMF=recode(VEMF,"Plantation"="植林","Agroforestry"="アグロフォレストリー",
                     "Biochar"="バイオ炭","Soil carbon sequestration"="土壌炭素隔離")) %>% 
  mutate(Run=recode(Run,"new"="更新後","old"="更新前"))
df_CDR_gl2 <- df_CDR_gl %>% 
  filter((REMF %in% c("World"))) %>% 
  filter((YEMF %in% c("2100")))

df_CDR_gl_ch <- df_CDR_gl2 %>%
  group_by(REMF,YEMF,Run) %>%
  summarize(
    Value = sum(Value)
  )

df_CDR_R5 <- df_red_land %>%
  filter(VEMF %in% c("Plantation", "Agroforestry","Biochar","Soil carbon sequestration")) %>% 
  mutate(VEMF=recode(VEMF,"Plantation"="植林","Agroforestry"="アグロフォレストリー",
                     "Biochar"="バイオ炭","Soil carbon sequestration"="土壌炭素隔離")) %>% 
  mutate(Run=recode(Run,"new"="更新後","old"="更新前"))
df_CDR_R5 <- df_CDR_R5 %>% 
  filter((REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))) %>% 
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter((YEMF %in% c("2100")))


cols_land <- c(
  "植林"                = "#8DAE92",  # 落ち着いたグリーン
  "アグロフォレストリー"              = "#ff7f0e",  # グレー寄りのセージグリーン
  "バイオ炭"                   = "#1f77b4",  # ダークブラウン（炭のイメージ）
  "土壌炭素隔離" = "#B89B63"   # 落ち着いた黄土色
)

g4 <- df_CDR_gl %>%
  mutate(VEMF = factor(VEMF, levels = c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=Run,y=Value/1000, fill = VEMF),stat = "identity",color="black",size = 0.15)+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.75) +
  labs(tag="(a)",title = "Emissions from Land use change\n土地利用変化由来の排出量",x="Scenario", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~YEMF, nrow = 1, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = cols_land)+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 14.75,color="black"), axis.text.y = element_text(size = 14.25,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=16.25),
        strip.text.x = element_text(size = 14.5),legend.text = element_text(size =14.25),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),plot.tag =element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g4)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_更新前後_gl.png", plot=g4, width = 6.5, height = 5.75)

g5 <- df_CDR_R5 %>%
  mutate(REMF = factor(REMF, levels = c("World","OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=Run,y=Value/1000,fill=VEMF),stat = "identity",width = 0.8,position = "stack",color="black",size = 0.15)+
  labs(title = "",tag = "", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = cols_land) +
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 14.75,color="black"), axis.text.y = element_text(size = 14.25,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=16.25),
        strip.text.x = element_text(size = 14.5),legend.text = element_text(size =15.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),plot.tag =element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g5)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_更新前後_R5.png", plot=g5, width = 11.25, height = 6.15)

p_all2 <- (g4 / g5)+ plot_layout(heights  = c(0.65, 1))
ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/Mtigation_CDR.png", plot=p_all2,width = 10.25, height = 9.25)

df_CDR_gl_cum <- df_CDR_gl %>%
  filter((REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))) %>% 
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  mutate(YEMF = as.numeric(as.character(YEMF))) %>% 
  filter(!is.na(Value))

years  <-
  seq(min(df_CDR_gl_cum$YEMF), max(df_CDR_gl_cum$YEMF), by = 1)

df_full2 <- df_CDR_gl_cum %>%
  group_by(VEMF,REMF,Scenario,Run) %>%
  complete(YEMF = years) %>%
  arrange(VEMF,REMF,YEMF,Run)

# ③ 線形補完
df_CDR_gl_cum2 <- df_full2 %>%
  group_by(VEMF,REMF,Scenario,Run) %>%
  mutate(Value = zoo::na.approx(Value, YEMF, na.rm = FALSE)) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>% 
  ungroup() %>% 
  group_by(VEMF,REMF,Scenario,Run) %>%
  summarise(Value = sum(Value)) %>% 
  ungroup() 

df_CDR_gl_cum3 <- df_CDR_gl_cum2 %>%
  filter(REMF %in% c("World"))

g4_cum <- df_CDR_gl_cum3 %>%
  mutate(VEMF = factor(VEMF, levels = c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=Run,y=Value/1000, fill = VEMF),stat = "identity",color="black",size = 0.15)+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.75) +
  labs(tag="(a)",title = "Emissions from Land use change\n土地利用変化由来の排出量",x="Scenario", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  #facet_wrap(~YEMF, nrow = 1, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = cols_land)+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 14.75,color="black"), axis.text.y = element_text(size = 14.25,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=16.25),
        strip.text.x = element_text(size = 14.5),legend.text = element_text(size =14.25),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),plot.tag =element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g4_cum)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_更新前後_gl_cum.png", plot=g4_cum, width = 6.5, height = 5.75)

g5_cum <- df_CDR_gl_cum2 %>%
  mutate(REMF = factor(REMF, levels = c("World","OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("植林", "アグロフォレストリー","バイオ炭","土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=Run,y=Value/1000,fill=VEMF),stat = "identity",width = 0.8,position = "stack",color="black",size = 0.15)+
  labs(title = "",tag = "", x="", y = expression(paste('2030-2100 Reduction potential (Gt',{CO[2]},'eq)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = cols_land) +
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 14.75,color="black"), axis.text.y = element_text(size = 14.25,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=16.25),
        strip.text.x = element_text(size = 14.5),legend.text = element_text(size =15.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),plot.tag =element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g5_cum)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_更新前後_R5_cum.png", plot=g5_cum, width = 11.25, height = 5.50)

#CDR_累積値--------------------------------------------------------------------
df_CDR_cum <- df_red_land %>%
  filter(VEMF %in% c("Plantation", "Agroforestry","Biochar","Soil carbon sequestration")) %>%
  filter(!(REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))) %>% 
  mutate(YEMF = as.numeric(as.character(YEMF))) %>% 
  filter(!is.na(Value))

years  <-
  seq(min(df_CDR_cum$YEMF), max(df_CDR_cum$YEMF), by = 1)

df_full <- df_CDR_cum %>%
  group_by(VEMF,REMF,Scenario,Run) %>%
  complete(YEMF = years) %>%
  arrange(VEMF,REMF,YEMF,Run)

# ③ 線形補完
df_CDR_cum2 <- df_full %>%
  group_by(VEMF,REMF,Scenario,Run) %>%
  mutate(Value = zoo::na.approx(Value, YEMF, na.rm = FALSE)) %>%
  mutate(Value = if_else(is.na(Value), 0, Value)) %>% 
  ungroup() %>% 
  group_by(VEMF,REMF,Scenario,Run) %>%
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  group_by(Scenario,REMF,Run) %>%
  summarise(
    Value = sum(Value, na.rm = TRUE)
  ) %>%
  mutate(VEMF = "CDR")

df_CDR_cum_new <- filter(df_CDR_cum2,(Run %in% c("new")))
df_CDR_cum_old <- filter(df_CDR_cum2,(Run %in% c("old")))

world_data_cum <- world %>%
  left_join(df_CDR_cum_new, by = c("iso_a3_eh" = "REMF"))%>%
  mutate(
    cat = cut(
      Value,
      breaks = c(0, 250, 500, 1000, 2000, 3000, 5000),
      labels = c("0–250", "250–500", "500–1000", "1000–2000", "2000–3000","3000–"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

world_data_cum_old <- world %>%
  left_join(df_CDR_cum_old, by = c("iso_a3_eh" = "REMF"))%>%
  mutate(
    cat = cut(
      Value,
      breaks = c(0, 250, 500, 1000, 2000, 3000, 25000),
      labels = c("0–250", "250–500", "500–1000", "1000–2000", "2000–3000","3000–"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

color_bar2 <- c(
  "0–250"     = "#f7fcf5",  # 非常に薄い緑（ほぼ白）
  "250–500"     = "#d0f0b2",  # 淡い黄緑
  "500–1000"    = "#66bd63",  # 明るい緑
  "1000–2000"   = "#238443",  # 緑
  "2000–3000"  = "#006633",  # 濃い緑
  "3000–" = "#00441b"   # 最も深い緑（最大値）
)

g6<- world_data_cum %>% 
  ggplot() +
  geom_sf(aes(fill = cat),color = "grey20", size = 0.05) +
  #scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#f7fcf5", high = "#003300") +
  scale_fill_manual(
    name = "CDR\n(MtCO2eq/yr)",
    values = color_bar2,
    na.value = "lightgrey",
    drop = FALSE
  ) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag ="(a)")+
  #guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =11.25),
        legend.position = c(0.1, 0.325),legend.title = element_text(size =12.75,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank(),plot.margin = margin(t = 0, b = 0))
plot(g6)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_cum_new.png", plot=g6, ,width = 8.75, height = 3.75)

g7<- world_data_cum_old %>% 
  ggplot() +
  geom_sf(aes(fill = cat),color = "grey20", size = 0.05) +
  #scale_fill_gradient(name = expression(paste('Potential (Mt',{CO[2]},'eq/yr)',seq="")),low = "#f7fcf5", high = "#003300") +
  scale_fill_manual(
    name = "CDR\n(MtCO2eq/yr)",
    values = color_bar2,
    na.value = "lightgrey",
    drop = FALSE
  ) +
  theme_bw()+
  #ggtitle("(a)")+
  labs(tag ="(b)")+
  #guides(fill = guide_colorbar(barwidth = 1.1, barheight = 5.1))+
  theme(plot.title = element_blank(), 
        axis.text.x =  element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), element_blank(),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =11.25),
        legend.position = c(0.1, 0.325),legend.title = element_text(size =12.75,margin = margin(b = 10)),
        panel.grid.minor = element_line(color = NA), 
        #axis.line = element_line(colour="black"),
        #panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
        panel.border = element_blank(),plot.margin = margin(t = 0, b = 0))
plot(g7)

ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/CDR_cum_old.png", plot=g7, ,width = 8.75, height = 3.75)

p_all <- (g2 / g3)
ggsave(file = "../../output/thesis/4.3 CDR技術情報更新/Mtigation_CDR_2100.png", plot=p_all,width = 8.50, height = 7.25)