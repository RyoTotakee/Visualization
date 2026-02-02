library(dplyr)
library(ggplot2)
library(stringr)

data_dir   <- "../data"
output_dir <- "../output/IAMCTemp"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


# region_list <- c("R5REF", "R5OECD90_EU", "R5ASIA", "R5MAF", "R5LAM", "World")
region_list <- c("BRA","CAN","CHN","IND","JPN","TUR","USA","XE25","XER","XOC","XSE","XSA","XLM","CIS","XME","XNF","XAF","World")

variable_list <- c(
  "Ter_Bio_Bio_Int_Ind",
  "Ter_Bio_Bio_Int_Ind_Wei_NPP",
  "Lan_Cov_Cro",
  "Lan_Cov_Frs",
  "Lan_Cov_Pst",
  "Lan_Cov_Frs",
  "Lan_Cov_Frs_Aff_and_Ref",
  "Lan_Cov_Frs_Aff",
  "Lan_Cov_Frs_Ref",
  "Lan_Cov_Frs_Frs",
  "Lan_Cov_Frs_Man",
  "Lan_Cov_Frs_Man_Aff",
  "Lan_Cov_Frs_Man_Ref",
  "Lan_Cov_Frs_Nat_Frs",
  "Lan_Cov_Frs_Nat_Frs_Prm_Frs",
  "Lan_Cov_Frs_Nat_Frs_Sec_Frs",
  "Lan_Cov_Frs_Prm_Frs",
  "Lan_Cov_Frs_Sec_Frs",
  "Lan_Cov_Oth_Lan",
  "Lan_Cov_Oth_Nat_Lan",
  "Lan_Cov_Pst",
  "Lan_Cov_Cro_Cer",
  "Lan_Cov_Cro_Ric",
  "Lan_Cov_Cro_Whe",
  "Lan_Cov_Cro_Coa_gra",
  "Lan_Cov_Cro_Oil_See",
  "Lan_Cov_Cro_Oil_Cro",
  "Lan_Cov_Cro_Sug_Cro",
  "Lan_Cov_Cro_Oth_Cro",
  "Lan_Cov_Cro_Rai"
)

IAMCTemp <- rgdx.param(paste0("../data/Mergedcomparison.gdx"), "IAMCTemp") %>%
  rename(Scenario=i1,Region=i2,Variable=i3,Unit=i4,Year=i5,Value=value) %>%
  filter(Region %in% region_list)  %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list) 
  )

for (v in variable_list) {
  
  df_v <- IAMCTemp %>% filter(Variable == v)
  
  if (nrow(df_v) == 0) {
    warning("No data for Variable: ", v, " (skip)")
    next
  }
  
  units <- df_v %>% distinct(Unit) %>% pull(Unit) %>% as.character()
  unit_label <- if (length(units) == 1) units else paste(units, collapse = " / ")
  y_lab <- paste0(v, " [", unit_label, "]")
  
  p <- ggplot(df_v, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    
    # ★Facet：縦軸をRegionごとに自由に
    facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
    
    labs(
      title = v,
      x = "Year",
      y = y_lab,
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