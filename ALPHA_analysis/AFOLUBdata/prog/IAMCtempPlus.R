library(dplyr)
library(ggplot2)
library(stringr)
library(gdxrrw)

data_dir   <- "../data"
output_dir <- "../output/IAMCtempPlus2"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# region_list <- c("R5REF", "R5OECD90_EU", "R5ASIA", "R5MAF", "R5LAM", "World")
region_list <- c("BRA","CAN","CHN","IND","JPN","TUR","USA","XE25","XER","XOC","XSE","XSA","XLM","CIS","XME","XNF","XAF","World")

variable_list <- c(
  "Agr_Pro_Coa_gra",
  "Agr_Pro_Cro_NonEne",
  "Agr_Pro_Liv",
  "Agr_Pro_Liv_Dai",
  "Agr_Pro_Liv_NonRum",
  "Agr_Pro_Liv_Rum",
  "Agr_Pro_NonEneCro",
  "Agr_Pro_NonEneLiv",
  "Agr_Pro_Oil_See",
  "Agr_Pro_Prc_Coa_gra",
  "Agr_Pro_Prc_Oil_See",
  "Agr_Pro_Prc_Ric",
  "Agr_Pro_Prc_Whe",
  "Agr_Pro_Ric",
  "Agr_Pro_Whe",
  "Car_Seq_Lan_Use_Soi_Car_Man",
  "Car_Seq_Lan_Use_Soi_Car_Man_Cro",
  "Car_Seq_Lan_Use_Soi_Car_Man_Gra",
  "Emi_CH4_AFO_Agr",
  "Emi_CH4_AFO_Agr_Liv",
  "Emi_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_CH4_AFO_Agr_Ric",
  "Emi_CO2_AFO_Aff",
  "Emi_CO2_AFO_Lan",
  "Emi_CO2_AFO_Lan_Cha_For_Woo",
  "Emi_CO2_AFO_Lan_For_Bur",
  "Emi_CO2_AFO_Lan_For_Gra_Con",
  "Emi_CO2_AFO_Lan_Pea_Fir_For",
  "Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff",
  "Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
  "Emi_CO2_Lan_Use_Flo_RemForest",
  "Emi_equ_CH4_AFO_Agr",
  "Emi_equ_CH4_AFO_Agr_Liv",
  "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_equ_CH4_AFO_Agr_Ric",
  "Emi_equ_CH4_AFO_Lan",
  "Emi_equ_CH4_AFO_Lan_For_Bur",
  "Emi_equ_CH4_AFO_Lan_Pea_Fir_For",
  "Emi_equ_GHG_AFO",
  "Emi_equ_GHG_AFO_Agr",
  "Emi_equ_GHG_AFO_Lan",
  "Emi_equ_N2O_AFO_Agr",
  "Emi_equ_N2O_AFO_Agr_Liv",
  "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_equ_N2O_AFO_Agr_Man_Soi",
  "Emi_equ_N2O_AFO_Lan",
  "Emi_equ_N2O_AFO_Lan_For_Bur",
  "Emi_equ_N2O_AFO_Lan_Pea_Fir_For",
  "Emi_GHG_AFO_Neg_Emi",
  "Emi_GHG_AFO_Pos_Emi",
  "Emi_GHG_AFO_Pos_Emi_Lan",
  "Emi_GHG_AFO_Red",
  "Emi_GHG_AFO_Seq",
  "Emi_N2O_AFO_Agr",
  "Emi_N2O_AFO_Agr_Liv",
  "Emi_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_N2O_AFO_Agr_Man_Soi",
  "Emi_Red_CH4_AFO_Agr",
  "Emi_Red_CH4_AFO_Agr_Liv",
  "Emi_Red_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_Red_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_Red_CH4_AFO_Agr_Ric",
  "Emi_Red_CO2_AFO_Lan",
  "Emi_Red_CO2_AFO_Lan_Agf",
  "Emi_Red_CO2_AFO_Lan_Bch",
  "Emi_Red_CO2_AFO_Lan_Enr",
  "Emi_Red_CO2_AFO_Lan_Pff",
  "Emi_Red_CO2_AFO_Lan_Pla",
  "Emi_Red_CO2_AFO_Lan_Pro",
  "Emi_Red_CO2_AFO_Lan_Res",
  "Emi_Red_CO2_AFO_Lan_Scc",
  "Emi_Red_CO2_AFO_Lan_Scg",
  "Emi_Red_CO2_AFO_Lan_Scs",
  "Emi_Red_equ_CH4_AFO_Agr",
  "Emi_Red_equ_CH4_AFO_Agr_Liv",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl",
  "Emi_Red_equ_CH4_AFO_Agr_Ric",
  "Emi_Red_equ_CH4_AFO_Agr_Ric_Res",
  "Emi_Red_equ_CH4_AFO_Agr_Ric_Wat",
  "Emi_Red_equ_GHG_AFO",
  "Emi_Red_equ_GHG_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Lan",
  "Emi_Red_equ_N2O_AFO_Agr",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til",
  "Emi_Red_N2O_AFO_Agr",
  "Emi_Red_N2O_AFO_Agr_Man_Soi",
  "Emi_Red_Rat_CH4_AFO_Agr",
  "Emi_Red_Rat_CO2_AFO_Lan",
  "Emi_Red_Rat_GHG_AFO_Agr",
  "Emi_Red_Rat_N2O_AFO_Agr",
  "Fer_Nit_Its",
  "Fer_Use_Nit",
  "GDP_MER",
  "Irr_Rat",
  "Lan_Are",
  "Lan_Are_Agf",
  "Lan_Are_Bch",
  "Lan_Are_Enr",
  "Lan_Are_Pff",
  "Lan_Are_Pla",
  "Lan_Are_Scc",
  "Lan_Are_Scg",
  "Lan_Cov_Cro",
  "Lan_Cov_Cro_Cer",
  "Lan_Cov_Cro_Coa_gra",
  "Lan_Cov_Cro_Ene_Cro",
  "Lan_Cov_Cro_Irr",
  "Lan_Cov_Cro_Non_Ene_Cro",
  "Lan_Cov_Cro_Oil_See",
  "Lan_Cov_Cro_Ric",
  "Lan_Cov_Cro_Whe",
  "Lan_Cov_Frs",
  "Lan_Cov_Frs_Man",
  "Lan_Cov_Frs_Unm",
  "Lan_Cov_Oth_Lan",
  "Lan_Cov_Pst",
  "Mar_Cos_GHG_AFO_Agr",
  "Mar_Cos_GHG_AFO_Lan",
  "POP",
  "Prc_Agr_Cor_Ind",
  "Prc_Agr_Soy_Ind",
  "Prc_Agr_Whe_Ind",
  "Red_Cos_AFO_Agr",
  "Red_Cos_AFO_Agr_CH4",
  "Red_Cos_AFO_Agr_Liv",
  "Red_Cos_AFO_Agr_Liv_Ent_Fer",
  "Red_Cos_AFO_Agr_Liv_Ent_Fer_Bre",
  "Red_Cos_AFO_Agr_Liv_Ent_Fer_Fee",
  "Red_Cos_AFO_Agr_Liv_Man_Man",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Adc",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Ams",
  "Red_Cos_AFO_Agr_Liv_Man_Man_And",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Cad",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Cfl",
  "Red_Cos_AFO_Agr_Man_Soi",
  "Red_Cos_AFO_Agr_Man_Soi_Fer",
  "Red_Cos_AFO_Agr_Man_Soi_Nin",
  "Red_Cos_AFO_Agr_Man_Soi_Res",
  "Red_Cos_AFO_Agr_Man_Soi_Slo",
  "Red_Cos_AFO_Agr_N2O",
  "Red_Cos_AFO_Agr_Ric",
  "Red_Cos_AFO_Agr_Ric_Res",
  "Red_Cos_AFO_Lan",
  "Red_Cos_AFO_Lan_Agf",
  "Red_Cos_AFO_Lan_Bch",
  "Red_Cos_AFO_Lan_Enr",
  "Red_Cos_AFO_Lan_Pff",
  "Red_Cos_AFO_Lan_Pla",
  "Red_Cos_AFO_Lan_Scc",
  "Red_Cos_AFO_Lan_Scg",
  "Yie_Cer",
  "Yie_Oil",
  "Yie_Sug"
)


IAMCTempPlus <- rgdx.param(paste0("../data/IAMC_templatePlus.gdx"), "IAMC_templatePlus") %>%
  rename(Scenario=Scenario,Region=REMF,Variable=VEMF,Year=YEMF,Value=IAMC_templatePlus) %>%
  filter(Region %in% region_list)  %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list) 
  )

for (v in variable_list) {
  
  df_v <- IAMCTempPlus %>% filter(Variable == v)
  
  if (nrow(df_v) == 0) {
    warning("No data for Variable: ", v, " (skip)")
    next
  }

  
  p <- ggplot(df_v, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    
    # ★Facet：縦軸をRegionごとに自由に
    facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
    
    labs(
      title = v,
      x = "Year",
      color = "Scenario"
    ) +
    
    theme_classic(base_size = 11) +
    
    # ★見やすさ調整（薄いグリッド + パネル枠 + ストリップ）
    theme(
      legend.position = "bottom",
      
      # 薄い目盛り線（major/minor）
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      
      # Facetの枠
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
      
      # ストリップ（Facet見出し）
      strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.4),
      strip.text = element_text(size = 10),
      
      # 余白
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      plot.title = element_text(face = "bold"),
      panel.spacing = unit(0.8, "lines")
    )
  
  safe_v <- v %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_")
  
  out_file <- file.path(output_dir, paste0(safe_v, ".png"))
  ggsave(out_file, p, width = 13, height = 8.5, dpi = 220, bg = "white")
  message("Saved: ", out_file)
}






data_dir   <- "../data"
output_dir <- "../output/IAMCtempPlusCompare"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1) New ----
IAMCTempPlus_new <- rgdx.param(paste0("../data/IAMC_templatePlus.gdx"), "IAMC_templatePlus") %>%
  rename(
    Scenario = Scenario,
    Region   = REMF,
    Variable = VEMF,
    Year     = YEMF,
    Value    = IAMC_templatePlus
  ) %>%
  mutate(
    Year   = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list),
    Source = "new"
  ) %>%
  filter(Region %in% region_list, Scenario == "SSP2_500C_CACN_DAC_NoCC") %>%
  select(-Scenario)

# ---- 2) Old ----
IAMCTempPlus_old <- rgdx.param(paste0("../data/IAMC_templatePlus_0127.gdx"), "IAMC_templatePlus") %>%
  rename(
    Scenario = Scenario,
    Region   = REMF,
    Variable = VEMF,
    Year     = YEMF,
    Value    = IAMC_templatePlus
  ) %>%
  mutate(
    Year   = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list),
    Source = "old"
  ) %>%
  filter(Region %in% region_list, Scenario == "SSP2_CM_NoCC") %>%
  select(-Scenario)

# ---- 3) bind ----
IAMCTempPlus_both <- bind_rows(IAMCTempPlus_new, IAMCTempPlus_old)

# ---- 4) plot & save (Variableごと) ----
for (v in variable_list) {

  df_v <- IAMCTempPlus_both %>% filter(Variable == v)

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

  out_file <- file.path(output_dir, paste0(safe_v, "_new_vs_old.png"))
  ggsave(out_file, p, width = 13, height = 8.5, dpi = 220, bg = "white")
  message("Saved: ", out_file)
}
