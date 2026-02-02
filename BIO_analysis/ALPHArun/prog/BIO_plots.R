library(terra)
library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)

if (!dir.exists("../output")) {
  dir.create("../output")
}

sce_list <- c("Baseline_LU_Fmig_Hub","Baseline_LU_Fmig_ALPHA")
sce <- "Baseline_LU_Fmig_Hub"

years <- c(2030,2050,2070,2090)

for (year in years) {
  r <- rast(paste0("../data/",sce,"/SPnumber/SPnumber_all_taxon_Baseline_",year,"_LU.tif"))
  df <- as.data.frame(r, xy = TRUE)
  colnames(df)[3] <- "value"
  
  g <- ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    coord_equal(expand = FALSE) +
    scale_fill_viridis_c(
      name = "Value",
      na.value = "grey90"
    ) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 11) +        # ← 背景を白に統一
    theme(
      panel.grid = element_blank(),   # 余計なグリッドを消す
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      
      # 凡例の調整
      legend.position   = "right",    # "bottom" もOK
      legend.title      = element_text(size = 10),
      legend.text       = element_text(size = 8),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.4, "cm"),
      
      # 余白調整
      plot.margin = margin(5, 5, 5, 5)
    )
  
  ggsave(filename = paste0("../output/",sce,"_",year,".png"),plot = g,width = 8,height = 4,dpi = 300)  
}



ESHI_list <- lapply(sce_list, function(sce) {
  df <- read.csv(
    paste0("../data/", sce, "/ESH/ESHI_RegionAgg_Baseline-MRI-ESM2-0-ssp370-LU_Fmig.csv")
  )
  df$source <- sce 
  df
})

ESHI_data <- do.call(rbind, ESHI_list)
ESHI_long_all <- ESHI_data %>%
  pivot_longer(
    cols = c(Current, X2030, X2050, X2070, X2090),
    names_to  = "Year_raw",
    values_to = "ESHI"
  ) %>%
  mutate(
    Year = if_else(
      Year_raw == "Current",
      2005L,
      as.integer(sub("^X", "", Year_raw))
    ),
    region = factor(
      region,
      levels = c("R5REF", "R5OECD90_EU", "R5ASIA", "R5MAF", "R5LAM", "World")
    )
  ) %>%
  select(-Year_raw)

indicator_list <- unique(ESHI_long_all$indicator)

for (ind in indicator_list) {
  dat_i <- ESHI_long_all %>%
    filter(indicator == ind)
  
  g_line <- ggplot(
    dat_i,
    aes(x = Year, y = ESHI, color = source, group = source)
  ) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~ region, nrow = 2) +
    scale_x_continuous(
      breaks = c(2005, 2030, 2050, 2070, 2090)
    ) +
    labs(
      title = ind,
      x = "Year",
      y = "ESHI",
      color = "Scenario"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(linewidth = 0.2),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold"),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.key.width  = unit(1.1, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  ggsave(
    filename = paste0("../output/ESHI_", ind, ".png"),
    plot     = g_line,
    width    = 9,
    height   = 6,
    dpi      = 300
  )
}