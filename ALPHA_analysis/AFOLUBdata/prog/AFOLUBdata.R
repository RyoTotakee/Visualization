library(dplyr)
library(ggplot2)
library(stringr)
library(gdxrrw)

data_dir   <- "../data"
output_dir <- "../output/AFOLUBdata"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# region_list <- c("R5REF", "R5OECD90_EU", "R5ASIA", "R5MAF", "R5LAM", "World")
# region_list <- c("BRA","CAN","CHN","IND","JPN","TUR","USA","XE25","XER","XOC","XSE","XSA","XLM","CIS","XME","XNF","XAF","World")
region_list <- c(
  # Asia
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "VNM",
  # Americas / Europe / Oceania
  "USA", "CAN", "MEX", "BRA", "ARG",
  "DEU", "FRA", "GBR", "ITA", "ESP",
  "AUS", "NZL",
  # Africa
  "ZAF", "EGY", "KEN", "ETH", "TZA", "NGA",
  # Middle East
  "SAU", "IRN", "TUR","IRL"
)

variable_list <- c(
  "Red_Cos_AFO_Agr_CH4",
  "Red_Cos_AFO_Agr_N2O",
  "Red_Cos_AFO_Agr_Liv_Ent_Fer",
  "Red_Cos_AFO_Agr_Liv_Man_Man",
  "Red_Cos_AFO_Agr_Man_Soi",
  "Red_Cos_AFO_Agr_Ric",
  "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_equ_CH4_AFO_Agr_Ric",
  "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_equ_N2O_AFO_Agr_Man_Soi",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_Red_equ_CH4_AFO_Agr_Ric",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi",
  "Emi_equ_GHG_AFO_Agr",
  "Emi_equ_GHG_AFO_Lan",
  "Emi_Red_equ_GHG_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Lan"
)

VEMF_MAP <- rgdx.set(paste0("../data/AFOLUBdata.gdx"), "VEMF_MAP") 

AFOLUBdata <- rgdx.param(paste0("../data/AFOLUBdata.gdx"), "AFOLUBdata") %>%
  rename(Value=AFOLUBdata) %>%
  left_join(
    VEMF_MAP %>% 
      select(VEMF, Var, Item, gas, Tech),
    by = c("Var", "Item", "gas", "Tech")
  ) %>%
  select(-Var,-Item,-gas,-Tech) %>%
  rename(Scenario=SCE_AFOLUB,Region=cty,Variable=VEMF,Year=yr) %>%
  filter(Region %in% region_list) %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list) 
  ) %>%
  filter(Scenario=="SSP2_CM_NoCC") %>%
  select(-Scenario)

AFOLUBdata_ann <- rgdx.param(paste0("../data/AFOLUBdata.gdx"), "AFOLUBdata_ann") %>%
  rename(Value=AFOLUBdata_ann) %>%
  left_join(
    VEMF_MAP %>% 
      select(VEMF, Var, Item, gas, Tech),
    by = c("Var", "Item", "gas", "Tech")
  ) %>%
  select(-Var,-Item,-gas,-Tech) %>%
  rename(Scenario=SCE_AFOLUB,Region=cty,Variable=VEMF,Year=yr) %>%
  filter(Region %in% region_list) %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list) 
  ) %>%
  filter(Scenario=="SSP2_CM_NoCC") %>%
  select(-Scenario)


# 1) 2つのデータに Source を付けて縦に結合
AFOLUB_both <- bind_rows(
  AFOLUBdata     %>% mutate(Source = "AFOLUBdata"),
  AFOLUBdata_ann %>% mutate(Source = "AFOLUBdata_ann")
)

# 2) Variableごとに保存（variable_list を使う想定）
for (v in variable_list) {
  
  df_v <- AFOLUB_both %>% filter(Variable == v)
  
  if (nrow(df_v) == 0) {
    warning("No data for Variable: ", v, " (skip)")
    next
  }
  
  p <- ggplot(df_v, aes(x = Year, y = Value, color = Source, group = Source)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
    labs(
      title = v,
      x = "Year",
      color = "Source"
    ) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "bottom",
      
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
      
      strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.4),
      strip.text = element_text(size = 10),
      
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      plot.title = element_text(face = "bold"),
      panel.spacing = unit(0.8, "lines")
    )
  
  safe_v <- v %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_")
  
  out_file <- file.path(output_dir, paste0(safe_v, "_AFOLUB_compare.png"))
  ggsave(out_file, p, width = 13, height = 8.5, dpi = 220, bg = "white")
  message("Saved: ", out_file)
}

