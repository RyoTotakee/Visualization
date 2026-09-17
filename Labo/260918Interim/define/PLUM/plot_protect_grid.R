#!/usr/bin/env Rscript

# AIM/PLUM `_protect` policy maps on the native 0.5-degree grid.
#
# This script plots the policy inputs and the pre-land-cap policy target.  The
# exact model-effective `protectfrac` additionally depends on the previous
# PLUM solution Y_pre (PRM_SEC, SL and OL) and must be read from each regional
# 2030 output GDX when a complete global run is available.

suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(sf)
  library(scales)
})

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else "plot_protect_grid.R"
script_dir <- dirname(normalizePath(script_file, mustWork = FALSE))

plum_dir <- if (length(args) >= 1) {
  normalizePath(args[1], mustWork = TRUE)
} else {
  normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
}
out_dir <- if (length(args) >= 2) args[2] else file.path(plum_dir, "R", "output", "protect_grid")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gams_candidates <- c(
  Sys.getenv("GAMS_SYSDIR"),
  "/home/rtotake/gams44.4_linux_x64_64_sfx",
  "C:/GAMS/44"
)
gams_dir <- gams_candidates[nzchar(gams_candidates) & dir.exists(gams_candidates)][1]
if (is.na(gams_dir)) stop("Set GAMS_SYSDIR to a GAMS system directory containing the GDX API.")
igdx(gams_dir)

policy_gdx <- file.path(plum_dir, "data", "policydata.gdx")
pop_gdx <- file.path(plum_dir, "data", "ssppopmap.gdx")
grid_gdx <- file.path(plum_dir, "data", "data_prep.gdx")
forest_yield_gdx <- file.path(plum_dir, "data", "visit_forest_growth_function.gdx")
bio_yield_gdx <- file.path(plum_dir, "data", "H08_yield_num_gdx.gdx")
coast_file <- file.path(
  plum_dir, "individual", "GCBB_biopotential",
  "ne_110m_coastline", "ne_110m_coastline.shp"
)

read_g <- function(gdx, symbol, value_name) {
  x <- rgdx.param(gdx, symbol)
  names(x)[1:2] <- c("G", value_name)
  x %>% transmute(G = as.integer(as.character(G)), !!value_name := as.numeric(.data[[value_name]]))
}

grid <- rgdx.set(grid_gdx, "MAP_GIJ") %>%
  transmute(
    G = as.integer(as.character(G)),
    I = as.integer(as.character(I)),
    J = as.integer(as.character(J)),
    lon = -180 + (J - 0.5) * 0.5,
    lat = 90 - (I - 0.5) * 0.5
  )

protect_bs <- read_g(policy_gdx, "protect_bs", "protect_bs")
protect_all <- read_g(policy_gdx, "protect_all", "protect_all")
degraded <- read_g(policy_gdx, "serious_land_allpolicy", "serious_land_allpolicy")

popdens <- rgdx.param(pop_gdx, "popdens") %>%
  filter(as.character(Y) == "2030", as.character(SSP) == "SSP2") %>%
  transmute(G = as.integer(as.character(G)), popdens = as.numeric(popdens))

# Current afforestation option in LandUseModel_MCP.gms is afftype=ccur_vst,
# which selects ACFout("AFRCUR",G), in MgC ha-1 yr-1.
aff_yield <- rgdx.param(forest_yield_gdx, "ACFout") %>%
  filter(as.character(LAFR) == "AFRCUR") %>%
  transmute(
    G = as.integer(as.character(G)),
    aff_yield_afrcur = as.numeric(ACFout)
  )

# PLUM defines YIELDBIO_H08 as the mean rainfed Miscanthus/switchgrass yield,
# converting the H08 inputs from kg ha-1 to tonne ha-1.
miscanthus <- rgdx.param(bio_yield_gdx, "yield_miscanthus_rainfed_num") %>%
  transmute(I = as.integer(as.character(i)), J = as.integer(as.character(j)),
            miscanthus = as.numeric(value))
switchgrass <- rgdx.param(bio_yield_gdx, "yield_swichgrass_rainfed_num") %>%
  transmute(I = as.integer(as.character(i)), J = as.integer(as.character(j)),
            switchgrass = as.numeric(value))
bio_yield <- full_join(miscanthus, switchgrass, by = c("I", "J")) %>%
  mutate(
    across(c(miscanthus, switchgrass), ~coalesce(.x, 0)),
    bio_yield_h08 = (miscanthus + switchgrass) / 2 / 1000
  ) %>%
  select(I, J, bio_yield_h08)

dat <- grid %>%
  left_join(protect_bs, by = "G") %>%
  left_join(protect_all, by = "G") %>%
  left_join(degraded, by = "G") %>%
  left_join(popdens, by = "G") %>%
  left_join(aff_yield, by = "G") %>%
  left_join(bio_yield, by = c("I", "J")) %>%
  mutate(
    across(c(protect_bs, protect_all, serious_land_allpolicy, popdens,
             aff_yield_afrcur, bio_yield_h08), ~coalesce(.x, 0)),
    # Same policy-target construction used in ALPHA/prog/0_grid_data.R.
    # In PLUM itself, a low-density cell protects all *existing PRM_SEC*, not
    # literally 100% of the cell; the Y_pre cap is applied in the model.
    target_bau = if_else(popdens < 0.1, 1, pmin(1, protect_bs)),
    target_protect = if_else(
      popdens < 0.1,
      1,
      pmin(1, protect_all + serious_land_allpolicy)
    ),
    additional_target = pmax(0, target_protect - target_bau)
  )

coast <- st_read(coast_file, quiet = TRUE)

map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "bottom",
    legend.key.width = unit(30, "pt"),
    plot.margin = margin(4, 6, 4, 6)
  )

save_map <- function(value, title, filename, palette = c("#f7fcf5", "#238b45")) {
  z <- dat %>% transmute(lon, lat, value = .data[[value]]) %>% filter(value > 0)
  p <- ggplot() +
    geom_tile(data = z, aes(lon, lat, fill = value), width = 0.5, height = 0.5) +
    geom_sf(data = coast, inherit.aes = FALSE, colour = "grey20", linewidth = 0.15) +
    scale_fill_gradientn(
      colours = palette,
      limits = c(0, 1),
      oob = squish,
      labels = label_percent(accuracy = 1),
      name = NULL,
      na.value = "transparent"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
    labs(title = title) +
    map_theme
  ggsave(file.path(out_dir, filename), p, width = 10, height = 4.8, dpi = 220, bg = "white")
}

save_yield_map <- function(value, title, filename, palette) {
  z <- dat %>% transmute(lon, lat, value = .data[[value]]) %>% filter(value > 0)
  upper <- max(z$value, na.rm = TRUE)
  p <- ggplot() +
    geom_tile(data = z, aes(lon, lat, fill = value), width = 0.5, height = 0.5) +
    geom_sf(data = coast, inherit.aes = FALSE, colour = "grey20", linewidth = 0.15) +
    scale_fill_gradientn(
      colours = palette,
      limits = c(0, upper),
      oob = squish,
      labels = label_number(accuracy = 0.1),
      name = NULL,
      na.value = "transparent"
    ) +
    coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
    labs(title = title) +
    map_theme
  ggsave(file.path(out_dir, filename), p, width = 10, height = 4.8, dpi = 220, bg = "white")
}

save_map("protect_all", "protect_all", "01_protect_all_input.png")
save_map(
  "serious_land_allpolicy", "serious_land_allpolicy",
  "02_serious_land_allpolicy_input.png", c("#fff7ec", "#d95f0e")
)
save_map(
  "target_protect", "protect_all + serious_land_allpolicy; popdens < 0.1",
  "03_protect_policy_target_2030.png", c("#f7fcf5", "#006d2c")
)
save_map(
  "additional_target", "_protect - BaU policy target",
  "04_protect_additional_vs_bau_2030.png", c("#fff5eb", "#a63603")
)
save_yield_map(
  "aff_yield_afrcur", 'ACFout("AFRCUR",G) [MgC ha-1 yr-1]',
  "05_afforestation_yield_afrcur.png", c("#ffffcc", "#238443")
)
save_yield_map(
  "bio_yield_h08", "YIELDBIO_H08(G) [t ha-1 yr-1]",
  "06_bioenergy_yield_h08.png", c("#fff7bc", "#d95f0e", "#7f2704")
)

write.csv(
  dat %>% select(G, I, J, lon, lat, protect_bs, protect_all,
                 serious_land_allpolicy, popdens, target_bau,
                 target_protect, additional_target,
                 aff_yield_afrcur, bio_yield_h08),
  file.path(out_dir, "protect_grid_2030.csv"),
  row.names = FALSE
)

message("Wrote maps and grid data to: ", normalizePath(out_dir))
