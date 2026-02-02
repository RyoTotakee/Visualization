library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(ggpmisc)
library(RColorBrewer)
library(purrr)
library(furrr)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
sf_use_s2(FALSE)




output_dir <- "../output/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

timeVals <- c(2015,2100) 
c_list <- c("wht", "rce", "crl", "mze", "str", "sgr", "swt", "pls", "nut", "ocr", "vol", "vgt", "frt", "stm", "spc", "alc", "cmt", "pmt", "omt", "rmt", "mlk", "dai", "egg", "ecr")


# c bas year
Yield <- rgdx.param(paste0("../data/SSP2_BaU_NoCC_World.gdx"), "Yld2_load") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))
head(Yield)

AnimalYield <- rgdx.param(paste0("../data/SSP2_BaU_NoCC_World.gdx"), "AnimalYld_load") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))
head(AnimalYield)

PL <- rgdx.param(paste0("../data/SSP2_BaU_NoCC_World.gdx"), "PL_load") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))
head(PL)

# country level
PP <- rgdx.param(paste0("../data/SSP2_BaU_NoCC_World.gdx"), "PP_load") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))
head(PP)

# baseyear only
GrassYld <- rgdx.param(paste0("../data/baseyear.gdx"), "GrassYld")
head(GrassYld)

grassusagerate <- rgdx.param(paste0("../data/baseyear.gdx"), "grass_usageratebase")
head(grassusagerate)

Wat_share <-rgdx.param(paste0("../data/baseyear.gdx"), "Wat_share") %>%
  filter(water == "Irrigation") %>%
  select(-water)
head(Wat_share)


# baseyear only and country level
TM <- rgdx.param(paste0("../data/baseyear.gdx"), "TMbase")
head(TM)

TE <- rgdx.param(paste0("../data/baseyear.gdx"), "TEbase")
head(TE)

PSE <- rgdx.param(paste0("../data/baseyear.gdx"), "PSEbase")
head(PSE)


# =========================================================
# Save both linear + log10 maps
# output_dir/varname/{linear|log}/commodity/*.png
# (NO year folders; year is in filename)
# =========================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(viridis)
  library(units)
  library(purrr)
  library(ragg)
})

sf::sf_use_s2(FALSE)

# timeVals <- c(2015,2050,2100)
# c_list   <- c("wht","rce",...,"ecr")
# output_dir <- "..."  # set this

# ---- Shapes (same as before) ----
Basin_Boundary <- st_read("../data/Basin_Boundary/Basin_Boundary.shp", quiet = TRUE) %>%
  mutate(
    ISO_A3 = as.character(ISO_A3),
    Basin  = as.character(Basin)
  )

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  transmute(
    ISO_A3 = as.character(iso_a3),
    adm0_a3 = as.character(adm0_a3),
    geometry
  ) %>%
  mutate(
    ISO_A3 = case_when(
      adm0_a3 == "FRA" ~ "FRA",
      adm0_a3 == "NOR" ~ "NOR",
      adm0_a3 == "KOS" ~ "XKX",
      TRUE ~ ISO_A3
    )
  ) %>%
  select(-adm0_a3) %>%
  filter(ISO_A3 != "ATA") %>%
  st_transform(st_crs(Basin_Boundary))

add_missing_from_world <- function(bb, world_sf){
  have_iso    <- unique(bb$ISO_A3)
  missing_iso <- setdiff(unique(world_sf$ISO_A3), have_iso)
  if (length(missing_iso) == 0) return(bb)
  
  missing_sf <- world_sf %>%
    filter(ISO_A3 %in% missing_iso) %>%
    mutate(Basin = NA_character_, Value = NA_real_) %>%
    select(ISO_A3, Basin, geometry, Value)
  
  bb %>%
    mutate(Value = NA_real_) %>%
    select(ISO_A3, Basin, geometry, Value) %>%
    rbind(missing_sf)
}

Basin_Boundary_aug <- add_missing_from_world(Basin_Boundary, world)

country_borders <- world %>%
  group_by(ISO_A3) %>%
  summarise(.groups = "drop") %>%
  sf::st_make_valid()

# ---- Plot templates (same design; add "trans" only) ----
basin_map_plot <- function(basin_map_df, title_txt, fill_txt, trans = NULL){
  ggplot() +
    geom_sf(data = world, fill = "grey85", color = NA) +
    geom_sf(data = basin_map_df, aes(fill = Value), color = NA) +
    geom_sf(data = country_borders, fill = NA, color = "white", linewidth = 0.5) +
    geom_sf(data = country_borders, fill = NA, color = "grey15", linewidth = 0.2) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey40", trans = trans) +
    labs(title = title_txt, fill = fill_txt) +
    theme_bw() +
    coord_sf(ylim = c(-60, 90), expand = FALSE) +
    theme(
      panel.background = element_rect(fill = "grey90", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = unit(12, "lines"),
        barheight = unit(0.7, "lines")
      )
    )
}

country_map_plot <- function(df_country, title_txt, fill_txt, trans = NULL){
  wdf <- world %>% left_join(df_country, by = c("ISO_A3" = "cty"))
  
  ggplot() +
    geom_sf(data = wdf, aes(fill = Value), color = NA) +
    geom_sf(data = country_borders, fill = NA, color = "white", linewidth = 0.5) +
    geom_sf(data = country_borders, fill = NA, color = "grey15", linewidth = 0.2) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey40", trans = trans) +
    labs(title = title_txt, fill = fill_txt) +
    theme_bw() +
    coord_sf(ylim = c(-60, 90), expand = FALSE) +
    theme(
      panel.background = element_rect(fill = "grey90", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    guides(
      fill = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        barwidth = unit(12, "lines"),
        barheight = unit(0.7, "lines")
      )
    )
}

# ---- Save: NO year folders; separate linear/log folders ----
save_plot <- function(p, varname, scale_tag, commodity, filename){
  outdir <- file.path(output_dir, varname, scale_tag, commodity)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(outdir, filename),
    plot = p,
    width = 12, height = 7,
    device = ragg::agg_png,
    dpi = 300
  )
}

# ---- Helper: make both linear + log versions (log: Value<=0 -> NA) ----
save_both_scales <- function(make_plot_fun, df_plot, varname, commodity, filename_base,
                             title_linear, title_log, fill_txt){
  
  # linear  ★ここだけ trans="identity" に
  p_lin <- make_plot_fun(df_plot, title_linear, fill_txt, trans = "identity")
  save_plot(p_lin, varname, "linear", commodity, paste0(filename_base, ".png"))
  
  # log10 (0/negative -> NA)
  df_log <- df_plot %>% mutate(Value = if_else(is.na(Value) | Value <= 0, NA_real_, Value))
  p_log <- make_plot_fun(df_log, title_log, fill_txt, trans = "log10")
  save_plot(p_log, varname, "log", commodity, paste0(filename_base, "_log10.png"))
}


# =========================================================
# 1) Basin-level, with year, with commodity: Yield / AnimalYield / PL
# columns: c, bas, cty, Y, <value>
# =========================================================
plot_basin_year_c <- function(df, varname, value_col){
  df <- df %>%
    mutate(
      c   = as.character(c),
      bas = as.character(bas),
      cty = as.character(cty),
      Y   = as.numeric(as.character(Y)),
      Value = as.numeric(.data[[value_col]])
    ) %>%
    filter(c %in% c_list, Y %in% timeVals)
  
  for (cc in sort(unique(df$c))){
    df_c <- df %>% filter(c == cc)
    for (yr in sort(unique(df_c$Y))){
      df_yr <- df_c %>% filter(Y == yr) %>% select(cty, bas, Value)
      
      basin_map_df <- Basin_Boundary_aug %>%
        select(-Value) %>%
        left_join(df_yr, by = c("ISO_A3" = "cty", "Basin" = "bas"))
      
      title_lin <- paste0(varname, " / ", cc, " (", yr, ")")
      title_log <- paste0(varname, " / ", cc, " (", yr, ", log10)")
      fname_base <- paste0(varname, "_", cc, "_", yr)
      
      save_both_scales(
        make_plot_fun = basin_map_plot,
        df_plot = basin_map_df,
        varname = varname,
        commodity = cc,
        filename_base = fname_base,
        title_linear = title_lin,
        title_log = title_log,
        fill_txt = value_col
      )
    }
  }
}

# =========================================================
# 2) Country-level, with year, with commodity: PP
# columns: c, cty, Y, <value>
# =========================================================
plot_country_year_c <- function(df, varname, value_col){
  df <- df %>%
    mutate(
      c   = as.character(c),
      cty = as.character(cty),
      Y   = as.numeric(as.character(Y)),
      Value = as.numeric(.data[[value_col]])
    ) %>%
    filter(c %in% c_list, Y %in% timeVals)
  
  for (cc in sort(unique(df$c))){
    df_c <- df %>% filter(c == cc)
    for (yr in sort(unique(df_c$Y))){
      df_yr <- df_c %>% filter(Y == yr) %>% select(cty, Value)
      
      title_lin <- paste0(varname, " / ", cc, " (", yr, ")")
      title_log <- paste0(varname, " / ", cc, " (", yr, ", log10)")
      fname_base <- paste0(varname, "_", cc, "_", yr)
      
      save_both_scales(
        make_plot_fun = country_map_plot,
        df_plot = df_yr,
        varname = varname,
        commodity = cc,
        filename_base = fname_base,
        title_linear = title_lin,
        title_log = title_log,
        fill_txt = value_col
      )
    }
  }
}

# =========================================================
# 3) Basin-level, baseyear only, no commodity: GrassYld / grassusagerate
# columns: bas, cty, <value>
# =========================================================
plot_basin_baseyear_noc <- function(df, varname, value_col){
  df0 <- df %>%
    mutate(
      bas = as.character(bas),
      cty = as.character(cty),
      Value = as.numeric(.data[[value_col]])
    ) %>%
    select(cty, bas, Value)
  
  basin_map_df <- Basin_Boundary_aug %>%
    select(-Value) %>%
    left_join(df0, by = c("ISO_A3" = "cty", "Basin" = "bas"))
  
  title_lin <- paste0(varname, " (baseyear)")
  title_log <- paste0(varname, " (baseyear, log10)")
  fname_base <- paste0(varname, "_baseyear")
  
  save_both_scales(
    make_plot_fun = basin_map_plot,
    df_plot = basin_map_df,
    varname = varname,
    commodity = "all",
    filename_base = fname_base,
    title_linear = title_lin,
    title_log = title_log,
    fill_txt = value_col
  )
}

# =========================================================
# 4) Basin-level, baseyear only, commodity is a_cro: Wat_share
# columns: a_cro, bas, cty, Wat_share
# =========================================================
plot_basin_baseyear_acro <- function(df, varname, value_col){
  df <- df %>%
    mutate(
      a_cro = as.character(a_cro),
      bas   = as.character(bas),
      cty   = as.character(cty),
      Value = as.numeric(.data[[value_col]])
    ) %>%
    filter(a_cro %in% c_list)
  
  for (cc in sort(unique(df$a_cro))){
    df_c <- df %>% filter(a_cro == cc) %>% select(cty, bas, Value)
    
    basin_map_df <- Basin_Boundary_aug %>%
      select(-Value) %>%
      left_join(df_c, by = c("ISO_A3" = "cty", "Basin" = "bas"))
    
    title_lin <- paste0(varname, " / ", cc, " (baseyear)")
    title_log <- paste0(varname, " / ", cc, " (baseyear, log10)")
    fname_base <- paste0(varname, "_", cc, "_baseyear")
    
    save_both_scales(
      make_plot_fun = basin_map_plot,
      df_plot = basin_map_df,
      varname = varname,
      commodity = cc,
      filename_base = fname_base,
      title_linear = title_lin,
      title_log = title_log,
      fill_txt = value_col
    )
  }
}

# =========================================================
# 5) Country-level, baseyear only, commodity: TM / TE / PSE
# columns: c, cty, <value>
# =========================================================
plot_country_baseyear_c <- function(df, varname, value_col){
  df <- df %>%
    mutate(
      c   = as.character(c),
      cty = as.character(cty),
      Value = as.numeric(.data[[value_col]])
    ) %>%
    filter(c %in% c_list)
  
  for (cc in sort(unique(df$c))){
    df_c <- df %>% filter(c == cc) %>% select(cty, Value)
    
    title_lin <- paste0(varname, " / ", cc, " (baseyear)")
    title_log <- paste0(varname, " / ", cc, " (baseyear, log10)")
    fname_base <- paste0(varname, "_", cc, "_baseyear")
    
    save_both_scales(
      make_plot_fun = country_map_plot,
      df_plot = df_c,
      varname = varname,
      commodity = cc,
      filename_base = fname_base,
      title_linear = title_lin,
      title_log = title_log,
      fill_txt = value_col
    )
  }
}

# =========================================================
# RUN (your objects)
# =========================================================
plot_basin_year_c(Yield,       "Yield",       "Yld2_load")
plot_basin_year_c(AnimalYield, "AnimalYield", "AnimalYld_load")
plot_basin_year_c(PL,          "PL",          "PL_load")

plot_country_year_c(PP, "PP", "PP_load")

plot_basin_baseyear_noc(GrassYld,       "GrassYld",       "GrassYld")
plot_basin_baseyear_noc(grassusagerate, "grassusagerate", "grass_usageratebase")

plot_basin_baseyear_acro(Wat_share, "Wat_share", "Wat_share")

plot_country_baseyear_c(TM,  "TM",  "TMbase")
plot_country_baseyear_c(TE,  "TE",  "TEbase")
plot_country_baseyear_c(PSE, "PSE", "PSEbase")
