#!/usr/bin/env Rscript

# Main figures for the 30by30 x land-based CDR analysis.
#
# Run from any directory:
#   Rscript publication/make_main_figures.R
#
# Optional:
#   TARGET_YEAR=2100 Rscript publication/make_main_figures.R

suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(ggrepel)
  library(grid)
  library(patchwork)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
  library(tidyr)
})

# Reproducible paths and settings -------------------------------------------

command_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)
if (length(script_arg) != 1L) {
  stop("Run this file with Rscript.")
}
script_path <- normalizePath(sub("^--file=", "", script_arg))
project_dir <- dirname(dirname(script_path))
setwd(project_dir)
if (!nzchar(Sys.getenv("TZ"))) {
  Sys.setenv(TZ = "Asia/Tokyo")
}

target_year <- as.integer(Sys.getenv("TARGET_YEAR", "2100"))
if (!target_year %in% c(2050L, 2100L)) {
  stop("TARGET_YEAR must be 2050 or 2100.")
}

data_dir <- file.path(project_dir, "data")
output_root <- file.path(project_dir, "output", "publication_figures")
source_data_root <- file.path(output_root, "source_data")
figure_dirs <- file.path(output_root, sprintf("figure_%d", 1:8))
supplementary_root <- file.path(output_root, "supplementary")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
dir.create(source_data_root, recursive = TRUE, showWarnings = FALSE)
dir.create(supplementary_root, recursive = TRUE, showWarnings = FALSE)
for (path in figure_dirs) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
unlink(file.path(
  source_data_root,
  c(
    "figure_1d_luc_effect.csv", "figure_1e_bii_effect.csv",
    "figure_3e_food_displacement_balance.csv",
    "figure_4abc_basin_yield_production.csv", "figure_4abcd_basin_yield_production.csv",
    "figure_4de_crp_decomposition.csv", "figure_4ef_crp_decomposition.csv",
    "figure_5_market_shapley.csv"
  )
))

init_gdx <- function() {
  candidates <- c(
    Sys.getenv("GAMS_SYS_DIR", unset = NA_character_),
    if (nzchar(Sys.which("gams"))) dirname(Sys.which("gams")) else NA_character_,
    Sys.glob(path.expand("~/gams*_linux_x64_64_sfx"))
  )
  candidates <- unique(candidates[!is.na(candidates)])
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0L) {
    stop("GAMS system directory not found. Set GAMS_SYS_DIR.")
  }
  gdxrrw::igdx(candidates[[1]], silent = TRUE)
}
init_gdx()
sf_use_s2(FALSE)

states <- c(
  `000` = "SSP2_BaU_NoCC_LandInt_global2",
  `100` = "SSP2_protect_BaU_NoCC_LandInt_global2",
  `010` = "SSP2NoBio_1100C_CACN_DAC_NoCC_LandInt_global2",
  `001` = "SSP2NoAff_1100C_CACN_DAC_NoCC_LandInt_global2",
  `110` = "SSP2NoBio_protect_1100C_CACN_DAC_NoCC_LandInt_global2",
  `101` = "SSP2NoAff_protect_1100C_CACN_DAC_NoCC_LandInt_global2",
  `011` = "SSP2_1100C_CACN_DAC_NoCC_LandInt_global2",
  `111` = "SSP2_protect_1100C_CACN_DAC_NoCC_LandInt_global2"
)
plum_states <- sub("global2$", "global1", states)
state_from_scenario <- function(x, state_vector = states) {
  unname(names(state_vector)[match(as.character(x), unname(state_vector))])
}

scenario_labels <- c(
  `000` = "BAU", `100` = "PROT", `010` = "MITI_NoBECCS",
  `001` = "MITI_NoPlantedForest", `110` = "MITI_PROT_NoBECCS",
  `101` = "MITI_PROT_NoPlantedForest", `011` = "MITI",
  `111` = "MITI_PROT"
)
scenario_colours <- c(
  `000` = "#2B2B2B", `100` = "#7B3294", `010` = "#008837",
  `001` = "#E08214", `110` = "#1B9E77", `101` = "#D95F02",
  `011` = "#2166AC", `111` = "#5E3C99"
)
policy_colours <- c(
  "Protection" = "#7B3294", "Planted forest" = "#1B9E77",
  "BECCS" = "#E08214"
)
r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)
food_crop_codes <- c(
  "wht", "rce", "crl", "mze", "str", "sgr", "pls",
  "nut", "ocr", "vgt", "frt", "stm", "spc"
)

land_colours <- c(
  "Food cropland" = "#D9A400", "Bioenergy cropland" = "#D95F02",
  "Fallow cropland" = "#F2D46F", "Forest" = "#1B9E77",
  "Pasture" = "#80B1D3", "Other natural land" = "#66C2A5",
  "Built-up area" = "#666666"
)

theme_pub <- function(base_size = 8.5) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(
      axis.title = element_text(colour = "black"),
      axis.text = element_text(colour = "black"),
      plot.title = element_text(face = "bold", size = rel(1.02), hjust = 0),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", colour = "black"),
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(3.2, "mm"),
      legend.key.width = unit(5.0, "mm"),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = .25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 5, 4, 5)
    )
}

theme_map <- function(base_size = 8.5) {
  theme_void(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.02), hjust = 0),
      strip.text = element_text(face = "bold", colour = "black"),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      plot.margin = margin(3, 3, 3, 3)
    )
}

tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 10, colour = "black"),
  plot.tag.position = c(0, 1),
  plot.title = element_text(margin = margin(l = 12))
)
small_map_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 8, colour = "black"),
  plot.tag.position = c(0, 1),
  plot.title = element_text(margin = margin(l = 11))
)

save_figure <- function(plot, figure_number, width, height) {
  stem <- sprintf("figure_%d", figure_number)
  directory <- figure_dirs[[figure_number]]
  ggsave(
    file.path(directory, paste0(stem, ".png")), plot,
    width = width, height = height, units = "in", dpi = 600,
    bg = "white", limitsize = FALSE
  )
  ggsave(
    file.path(directory, paste0(stem, ".pdf")), plot,
    width = width, height = height, units = "in",
    device = cairo_pdf, bg = "white", limitsize = FALSE
  )
}

symmetric_limit <- function(x, probability = .98) {
  x <- abs(x[is.finite(x) & x != 0])
  if (length(x) == 0L) return(1)
  max(unname(quantile(x, probability, na.rm = TRUE)), .Machine$double.eps)
}
sparse_pretty_breaks <- function(limits) {
  pretty(limits, n = 4)
}

world_sf <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(admin != "Antarctica")
coord_world_no_antarctica <- function() {
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE)
}
merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
merged_iamc <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  transmute(
    scenario = as.character(i1), geography = as.character(i2),
    variable = as.character(i3), year = as.integer(as.character(i4)),
    value = as.numeric(value), state = state_from_scenario(i1)
  ) %>%
  filter(!is.na(state))

# Figure 1: land-system change ----------------------------------------------

land_variables <- c(
  Lan_Cov = "Total land",
  Lan_Cov_Cro = "Total cropland",
  Lan_Cov_Cro_Non_Ene_Cro = "Food cropland",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "Bioenergy cropland",
  Lan_Cov_Frs = "Forest",
  Lan_Cov_Frs_Aff = "Planted forest",
  Lan_Cov_Pst = "Pasture",
  Lan_Cov_Oth_Nat_Lan = "Other natural land",
  Lan_Cov_Bui_Are = "Built-up area",
  Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int = "Protected footprint"
)
land_raw <- merged_iamc %>%
  filter(
    geography %in% c("WLD", names(r10_labels)),
    variable %in% names(land_variables),
    year >= 2020, year <= target_year
  ) %>%
  mutate(land_class = unname(land_variables[variable])) %>%
  group_by(state, geography, year, land_class) %>%
  summarise(area_mha = sum(value, na.rm = TRUE), .groups = "drop")

main_states <- c("000", "011", "111")
trajectory_classes <- c(
  "Food cropland", "Bioenergy cropland", "Planted forest"
)
fig1a_data <- land_raw %>%
  filter(
    geography == "WLD", state %in% main_states,
    land_class %in% trajectory_classes
  ) %>%
  mutate(
    land_class = factor(land_class, levels = trajectory_classes),
    scenario = factor(unname(scenario_labels[state]),
                      levels = unname(scenario_labels[main_states]))
  )
write_csv(fig1a_data, file.path(source_data_root, "figure_1a_global_land_trajectories.csv"))

p1a <- ggplot(
  fig1a_data,
  aes(year, area_mha, colour = state, linetype = state, group = state)
) +
  geom_line(linewidth = .65) +
  geom_point(size = .85, stroke = 0) +
  facet_wrap(vars(land_class), scales = "free_y", nrow = 1) +
  scale_colour_manual(
    values = scenario_colours[main_states], breaks = main_states,
    labels = scenario_labels[main_states], name = NULL
  ) +
  scale_linetype_manual(
    values = c(`000` = "solid", `011` = "dashed", `111` = "solid"),
    breaks = main_states, labels = scenario_labels[main_states], name = NULL
  ) +
  scale_x_continuous(breaks = seq(2020, target_year, by = 20)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Global land use", x = NULL, y = "Area (million ha)") +
  theme_pub() +
  theme(
    legend.position = "bottom", panel.grid.major.x = element_blank(),
    legend.box = "horizontal"
  )

exclusive_classes <- c(
  "Food cropland", "Bioenergy cropland", "Fallow cropland",
  "Forest", "Pasture", "Other natural land", "Built-up area"
)
required_land_classes <- c(
  "Total land", "Total cropland", "Food cropland",
  "Bioenergy cropland", "Forest", "Pasture",
  "Other natural land", "Built-up area"
)
make_land_wide <- function(selected_year) {
  expand_grid(
    state = main_states,
    geography = names(r10_labels),
    land_class = required_land_classes
  ) %>%
    left_join(
      land_raw %>%
        filter(
          geography %in% names(r10_labels), year == selected_year,
          land_class %in% required_land_classes
        ) %>%
        select(state, geography, land_class, area_mha),
      by = c("state", "geography", "land_class")
    ) %>%
    mutate(area_mha = replace_na(area_mha, 0)) %>%
    group_by(state, geography, land_class) %>%
    summarise(area_mha = sum(area_mha), .groups = "drop") %>%
    pivot_wider(names_from = land_class, values_from = area_mha, values_fill = 0) %>%
    mutate(
      `Fallow cropland` = `Total cropland` - `Food cropland` -
        `Bioenergy cropland`
    )
}

land_2020_wide <- make_land_wide(2020L)
land_target_wide <- make_land_wide(target_year)
baseline_2020_long <- land_2020_wide %>%
  select(state, geography, total_land_2020 = `Total land`, all_of(exclusive_classes)) %>%
  pivot_longer(
    all_of(exclusive_classes), names_to = "land_class",
    values_to = "area_2020_mha"
  )
target_long <- land_target_wide %>%
  select(state, geography, all_of(exclusive_classes)) %>%
  pivot_longer(
    all_of(exclusive_classes), names_to = "land_class",
    values_to = "area_target_mha"
  )
scenario_offsets <- c(`000` = -.24, `011` = 0, `111` = .24)
fig1b_data <- target_long %>%
  left_join(
    baseline_2020_long,
    by = c("state", "geography", "land_class")
  ) %>%
  mutate(
    region = factor(unname(r10_labels[geography]), levels = unname(r10_labels)),
    scenario = factor(
      unname(scenario_labels[state]), levels = unname(scenario_labels[main_states])
    ),
    land_class = factor(land_class, levels = exclusive_classes),
    change_mha = area_target_mha - area_2020_mha,
    change_pct_land = 100 * change_mha / total_land_2020,
    region_id = match(as.character(region), levels(region)),
    plot_x = region_id + unname(scenario_offsets[state]),
    baseline_year = 2020L,
    baseline_origin = "model output"
  )
write_csv(fig1b_data, file.path(source_data_root, "figure_1b_regional_land_change.csv"))

p1b <- ggplot(
  fig1b_data,
  aes(plot_x, change_pct_land, fill = land_class, colour = state)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .21, linewidth = .35) +
  scale_fill_manual(values = land_colours, name = NULL) +
  scale_colour_manual(
    values = scenario_colours[main_states], breaks = main_states,
    labels = scenario_labels[main_states], name = NULL
  ) +
  scale_x_continuous(
    breaks = seq_along(r10_labels), labels = unname(r10_labels),
    expand = expansion(mult = c(.025, .025))
  ) +
  scale_y_continuous(labels = label_number(accuracy = .1, suffix = "%")) +
  labs(
    title = paste0("Regional land change, 2020–", target_year),
    x = NULL, y = "Change (% of 2020 regional land)"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 6.5),
    legend.position = "bottom", legend.box = "horizontal",
    legend.box.just = "center",
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 6.8)
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE, order = 1),
    colour = guide_legend(
      nrow = 1, order = 2,
      override.aes = list(fill = NA, linewidth = .9)
    )
  )

fig1c_observed <- land_raw %>%
  filter(
    geography == "WLD", state %in% main_states, year >= 2020,
    land_class %in% c("Total land", "Protected footprint")
  ) %>%
  pivot_wider(names_from = land_class, values_from = area_mha) %>%
  filter(!is.na(`Protected footprint`)) %>%
  mutate(
    protected_share_pct = 100 * `Protected footprint` / `Total land`,
    value_origin = "model output"
  )
initial_protected_share <- fig1c_observed %>%
  filter(state == "011", year == min(year)) %>%
  pull(protected_share_pct) %>%
  first()
fig1c_initial <- land_raw %>%
  filter(
    geography == "WLD", state %in% main_states, year == 2020,
    land_class == "Total land"
  ) %>%
  transmute(
    state, geography, year, `Total land` = area_mha,
    `Protected footprint` = area_mha * initial_protected_share / 100,
    protected_share_pct = initial_protected_share,
    value_origin = "common 2020 baseline"
  )
fig1c_data <- bind_rows(fig1c_initial, fig1c_observed) %>%
  distinct(state, year, .keep_all = TRUE) %>%
  mutate(
    scenario = factor(unname(scenario_labels[state]),
                      levels = unname(scenario_labels[main_states]))
  )
write_csv(fig1c_data, file.path(source_data_root, "figure_1c_protected_share.csv"))

p1c <- ggplot(fig1c_data, aes(year, protected_share_pct, colour = state)) +
  geom_line(linewidth = .7) +
  geom_point(size = .9) +
  scale_colour_manual(
    values = scenario_colours[main_states], breaks = main_states,
    labels = scenario_labels[main_states], name = NULL
  ) +
  scale_x_continuous(breaks = seq(2020, target_year, by = 20)) +
  scale_y_continuous(
    limits = c(0, NA), labels = label_number(accuracy = 1, suffix = "%"),
    expand = expansion(mult = c(0, .04))
  ) +
  labs(title = "Protected land", x = NULL, y = "Share of total land") +
  theme_pub() +
  theme(legend.position = "none", panel.grid.major.x = element_blank())

# Cumulative LUC emissions and removals exclude managed-forest fluxes.
ghgl <- rgdx.param(merged_file, "GHGL") %>%
  transmute(
    scenario = as.character(i1), region = as.character(i2),
    year = as.integer(as.character(i3)), emission_category = as.character(i4),
    land_category = as.character(i5), value = as.numeric(value),
    state = state_from_scenario(i1)
  ) %>%
  filter(!is.na(state))
trapezoid_integral <- function(year, value) {
  order_id <- order(year)
  year <- year[order_id]
  value <- value[order_id]
  if (length(year) < 2L) return(NA_real_)
  sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)
}
fig1d_data <- ghgl %>%
  filter(
    region == "World", state %in% c("011", "111"),
    year >= 2020, year <= target_year,
    land_category == "LUC",
    emission_category %in% c("Positive", "Negative")
  ) %>%
  group_by(state, emission_category, year) %>%
  summarise(value_mtco2_yr = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(state, emission_category) %>%
  summarise(
    cumulative_gtco2 = trapezoid_integral(year, value_mtco2_yr) / 1000,
    .groups = "drop"
  ) %>%
  mutate(
    scenario = factor(unname(scenario_labels[state]),
                      levels = unname(scenario_labels[c("011", "111")])),
    component = factor(
      recode(emission_category, Positive = "Emissions", Negative = "Removals"),
      levels = c("Emissions", "Removals")
    )
  )
write_csv(fig1d_data, file.path(source_data_root, "figure_1d_cumulative_luc.csv"))

fig1d_net <- fig1d_data %>%
  group_by(state, scenario) %>%
  summarise(net_gtco2 = sum(cumulative_gtco2), .groups = "drop")

p1d <- ggplot(fig1d_data, aes(scenario, cumulative_gtco2, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = .3) +
  geom_col(width = .58) +
  geom_point(
    data = fig1d_net, aes(scenario, net_gtco2),
    inherit.aes = FALSE, shape = 18, size = 2
  ) +
  scale_fill_manual(values = c(Emissions = "#D6604D", Removals = "#4393C3"),
                    name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "Cumulative LUC flux", x = NULL,
       y = expression("Gt CO"[2])) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 6.8),
    legend.position = "bottom", panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 6.5)
  )

bii_years <- c(2005L, 2010L, seq(2020L, 2100L, by = 10L))
read_bii <- function(state) {
  path <- file.path(
    data_dir, "BII", "nc", paste0("BIImap_", states[[state]], "_default.nc")
  )
  rast(path, subds = "BII")[[match(target_year, bii_years)]]
}
fig1e_data <- lapply(c("011", "111"), function(state_id) {
  bii_layer <- read_bii(state_id)
  bii_area <- cellSize(bii_layer, unit = "km")
  valid_area <- ifel(is.na(bii_layer), NA, bii_area)
  tibble(
    state = state_id,
    bii = global(bii_layer * valid_area, "sum", na.rm = TRUE)[[1]] /
      global(valid_area, "sum", na.rm = TRUE)[[1]]
  )
}) %>% bind_rows() %>%
  mutate(
    scenario = factor(unname(scenario_labels[state]),
                      levels = unname(scenario_labels[c("011", "111")]))
  )
write_csv(fig1e_data, file.path(source_data_root, "figure_1e_bii_scenarios.csv"))

p1e <- ggplot(fig1e_data, aes(scenario, bii, colour = state, group = 1)) +
  geom_line(colour = "grey55", linewidth = .4) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = scenario_colours[c("011", "111")], guide = "none") +
  scale_y_continuous(labels = label_number(accuracy = .0001),
                     expand = expansion(mult = c(.15, .15))) +
  labs(title = paste0("BII in ", target_year), x = NULL, y = "Area-weighted BII") +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, size = 6.8),
    panel.grid.major.x = element_blank()
  )

figure_1 <- p1a / p1b / (p1c | p1d | p1e) +
  plot_layout(heights = c(1.00, 1.20, .8)) +
  plot_annotation(tag_levels = "a", theme = tag_theme)
save_figure(figure_1, 1, width = 7.2, height = 8.0)

# Shared AIM-LU grid data for Figures 2 and 3 -------------------------------

grid_years <- c(2005L, 2010L, seq(2020L, 2100L, by = 10L))

# ============================================================================
# TEMPORARY WORKAROUND FOR EXISTING NETCDF FILES ONLY
# The current AIM-LUmap files were written by a combine.gms version that added
# both Rbasin and its R17 aggregate to each grid cell.  Divide LC_area_share by
# two until all NetCDF files have been regenerated with the corrected
# combine.gms.  REMOVE this correction after that regeneration.
# ============================================================================
grid_export_duplicate_factor <- 2
land_class_index <- c(
  food = 1L, bio = 2L, natural_forest = 4L,
  planted_forest = 5L, natural_grassland_other = 9L
)
land_class_labels <- c(
  food = "Cropland", bio = "Bioenergy cropland",
  planted_forest = "Planted forest", natural_land = "Natural land"
)

read_land_grid <- function(state) {
  path <- file.path(
    data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[[state]], ".nc")
  )
  all_layers <- rast(path, subds = "LC_area_share")
  positions <- (unname(land_class_index) - 1L) * length(grid_years) +
    match(target_year, grid_years)
  selected <- all_layers[[positions]] / grid_export_duplicate_factor
  names(selected) <- names(land_class_index)
  result <- c(
    selected[["food"]], selected[["bio"]], selected[["planted_forest"]],
    selected[["natural_forest"]] + selected[["natural_grassland_other"]]
  )
  names(result) <- c("food", "bio", "planted_forest", "natural_land")
  result
}

grid_states_needed <- c("000", "100", "010", "001", "011", "111")
land_grids <- setNames(lapply(grid_states_needed, read_land_grid), grid_states_needed)
reference_nc <- file.path(
  data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[["011"]], ".nc")
)
pixel_area <- rast(reference_nc, subds = "pixel_area")
all_reference_layers <- rast(reference_nc, subds = "LC_area_share")
mask_positions <- (0:11) * length(grid_years) + match(target_year, grid_years)
land_mask <- app(all_reference_layers[[mask_positions]], sum, na.rm = TRUE)
land_mask[land_mask <= 0] <- NA

to_map_data <- function(x, value_name = "value") {
  data <- as.data.frame(mask(x, land_mask), xy = TRUE, na.rm = TRUE)
  names(data)[[3]] <- value_name
  data
}

# Figure 2: protection pressure and land response ---------------------------

basin_boundaries <- st_read(
  file.path(data_dir, "Basin_Boundary", "Basin_Boundary.shp"), quiet = TRUE
) %>%
  transmute(country = as.character(ISO_A3), basin = as.character(Basin))

basin_protection <- rgdx.param(
  file.path(data_dir, "ALPHA", "plum_data.gdx"), "protPlum"
) %>%
  filter(
    sce_plum %in% plum_states[c("011", "111")],
    as.integer(as.character(yr)) == target_year,
    lu_prot %in% c("primf", "secdf", "gra")
  ) %>%
  transmute(
    state = state_from_scenario(sce_plum, plum_states),
    land_type = as.character(lu_prot), basin = as.character(bas),
    country = as.character(cty), protected_share = as.numeric(protPlum)
  ) %>%
  complete(
    nesting(basin, country), state = c("011", "111"),
    land_type = c("primf", "secdf", "gra"),
    fill = list(protected_share = 0)
  ) %>%
  group_by(state, basin, country) %>%
  summarise(protected_share = mean(protected_share), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = protected_share, values_fill = 0) %>%
  transmute(
    basin, country, protection_increase_pp = 100 * (`111` - `011`)
  )
write_csv(basin_protection, file.path(source_data_root, "figure_2_basin_protection_pressure.csv"))

basin_protection_sf <- basin_boundaries %>%
  left_join(basin_protection, by = c("country", "basin"))
pressure_values <- basin_protection$protection_increase_pp
pressure_values <- pressure_values[is.finite(pressure_values) & pressure_values > 0]
pressure_limit <- unname(quantile(pressure_values, .98, na.rm = TRUE))

p2a <- ggplot() +
  geom_sf(
    data = basin_protection_sf, aes(fill = protection_increase_pp),
    colour = NA
  ) +
  geom_sf(data = world_sf, fill = NA, colour = "grey30", linewidth = .12) +
  scale_fill_gradient(
    low = "#F2F0F7", high = "#54278F", limits = c(0, pressure_limit),
    oob = squish, na.value = "grey94",
    name = "Mean increase\n(percentage points)"
  ) +
  coord_world_no_antarctica() +
  labs(title = "Added protected share by basin", tag = "a") +
  theme_map() + small_map_tag_theme

pressure_raster <- rasterize(
  vect(basin_protection_sf), pixel_area,
  field = "protection_increase_pp", background = NA, touches = TRUE
)
pressure_raster_values <- values(pressure_raster, mat = FALSE)
pressure_raster_values <- pressure_raster_values[
  is.finite(pressure_raster_values) & pressure_raster_values > 0
]
pressure_breaks <- unname(quantile(
  pressure_raster_values, c(1 / 3, 2 / 3), na.rm = TRUE
))
pressure_class <- ifel(
  pressure_raster <= pressure_breaks[[1]], 1,
  ifel(pressure_raster <= pressure_breaks[[2]], 2, 3)
)

bivariate_colours <- c(
  `1` = "#FEE5D9", `2` = "#FCAE91", `3` = "#CB181D",
  `4` = "#FAFAFA", `5` = "#EEEEEE", `6` = "#D9D9D9",
  `7` = "#DEEBF7", `8` = "#9ECAE1", `9` = "#2171B5"
)
stable_threshold_pp <- .1
write_csv(
  tibble(
    classification = c(
      "Added protected share: Low to Mid",
      "Added protected share: Mid to High",
      "Land-cover response: Decrease to Stable",
      "Land-cover response: Stable to Increase"
    ),
    threshold_percentage_points = c(
      pressure_breaks[[1]], pressure_breaks[[2]],
      -stable_threshold_pp, stable_threshold_pp
    )
  ),
  file.path(source_data_root, "figure_2_class_thresholds.csv")
)
land_delta_111_011 <- (land_grids[["111"]] - land_grids[["011"]]) * 100

make_bivariate_map <- function(class_id, title, tag) {
  delta_pp <- mask(land_delta_111_011[[class_id]], land_mask)
  response_class <- ifel(
    delta_pp < -stable_threshold_pp, 1,
    ifel(delta_pp > stable_threshold_pp, 3, 2)
  )
  code_layer <- mask(pressure_class + 3 * (response_class - 1), land_mask)
  map_data <- to_map_data(code_layer, "code") %>%
    mutate(code = factor(round(code), levels = 1:9))
  write_csv(
    map_data %>% transmute(x, y, class_code = as.integer(code)),
    file.path(source_data_root, paste0("figure_2_", class_id, "_bivariate_grid.csv.gz"))
  )
  ggplot(map_data, aes(x, y, fill = code)) +
    geom_tile(width = .5, height = .5) +
    geom_sf(
      data = world_sf, fill = NA, colour = "grey30", linewidth = .12,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = bivariate_colours, guide = "none", na.value = "grey94") +
    coord_world_no_antarctica() +
    labs(title = title, tag = tag) +
    theme_map() + small_map_tag_theme
}

p2b <- make_bivariate_map("planted_forest", "Planted forest", "b")
p2c <- make_bivariate_map("bio", "Bioenergy cropland", NULL)
p2d <- make_bivariate_map("food", "Cropland", NULL)
p2e <- make_bivariate_map("natural_land", "Natural land", NULL)

bivariate_legend_data <- expand_grid(
  pressure_id = 1:3, response_id = 1:3
) %>%
  mutate(
    code = factor(pressure_id + 3L * (response_id - 1L), levels = 1:9),
    pressure = factor(c("Low", "Medium", "High")[pressure_id],
                      levels = c("Low", "Medium", "High")),
    response = factor(c("Decrease", "Stable", "Increase")[response_id],
                      levels = c("Decrease", "Stable", "Increase"))
  )
p2legend <- ggplot(
  bivariate_legend_data, aes(pressure, response, fill = code)
) +
  geom_tile(colour = "white", linewidth = .6) +
  scale_fill_manual(values = bivariate_colours, guide = "none") +
  scale_x_discrete(labels = c(
    Low = paste0("Low\n≤", number(pressure_breaks[[1]], accuracy = .1)),
    Medium = paste0(
      "Mid\n", number(pressure_breaks[[1]], accuracy = .1), "–",
      number(pressure_breaks[[2]], accuracy = .1)
    ),
    High = paste0("High\n>", number(pressure_breaks[[2]], accuracy = .1))
  )) +
  scale_y_discrete(labels = c(
    Decrease = paste0("Decrease\n<−", number(stable_threshold_pp, accuracy = .1)),
    Stable = paste0("Stable\n−", number(stable_threshold_pp, accuracy = .1),
                    " to +", number(stable_threshold_pp, accuracy = .1)),
    Increase = paste0("Increase\n>", number(stable_threshold_pp, accuracy = .1))
  )) +
  coord_fixed(ratio = .65) +
  labs(
    x = "Added protected share (percentage points)",
    y = "Land-cover change (percentage points)"
  ) +
  theme_pub(6.1) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 5.4, lineheight = .9),
    axis.text.y = element_text(size = 5.4, lineheight = .9),
    axis.title = element_text(size = 5.8),
    plot.margin = margin(2, 8, 2, 8)
  )

p2legend_lower <- plot_spacer() / p2legend +
  plot_layout(heights = c(.08, .92))
p2_top <- (p2a | p2legend_lower) + plot_layout(widths = c(1.85, .65))
figure_2 <- p2_top / (p2b | p2c) / (p2d | p2e) +
  plot_layout(heights = c(1, 1, 1))
save_figure(figure_2, 2, width = 7.2, height = 6.4)

# Figure 3: displacement and spillover of cropland -------------------------

food_delta <- land_grids[["111"]][["food"]] -
  land_grids[["011"]][["food"]]
bio_delta <- land_grids[["111"]][["bio"]] -
  land_grids[["011"]][["bio"]]
forest_delta <- land_grids[["111"]][["planted_forest"]] -
  land_grids[["011"]][["planted_forest"]]
food_loss <- ifel(food_delta < 0, -food_delta, 0)
food_gain <- ifel(food_delta > 0, food_delta, 0)
bio_gain <- ifel(bio_delta > 0, bio_delta, 0)
forest_gain <- ifel(forest_delta > 0, forest_delta, 0)
bio_loss <- ifel(bio_delta < 0, -bio_delta, 0)
forest_loss <- ifel(forest_delta < 0, -forest_delta, 0)

candidate_destination_gain <- bio_gain + forest_gain
matched_food_loss <- ifel(
  candidate_destination_gain > 0,
  ifel(food_loss < candidate_destination_gain, food_loss, candidate_destination_gain),
  0
)
candidate_source_loss <- bio_loss + forest_loss
matched_food_gain <- ifel(
  candidate_source_loss > 0,
  ifel(food_gain < candidate_source_loss, food_gain, candidate_source_loss),
  0
)
food_signals <- c(
  ifel(candidate_destination_gain > 0,
       matched_food_loss * forest_gain / candidate_destination_gain, 0),
  ifel(candidate_destination_gain > 0,
       matched_food_loss * bio_gain / candidate_destination_gain, 0),
  food_loss - matched_food_loss,
  ifel(candidate_source_loss > 0,
       matched_food_gain * forest_loss / candidate_source_loss, 0),
  ifel(candidate_source_loss > 0,
       matched_food_gain * bio_loss / candidate_source_loss, 0),
  food_gain - matched_food_gain
)
names(food_signals) <- c(
  "cropland_to_forest", "cropland_to_bioenergy", "cropland_to_other",
  "forest_to_cropland", "bioenergy_to_cropland", "other_to_cropland"
)
food_signal_labels <- c(
  cropland_to_forest = "Cropland → planted forest",
  cropland_to_bioenergy = "Cropland → bioenergy",
  cropland_to_other = "Cropland → other land",
  forest_to_cropland = "Planted forest → cropland",
  bioenergy_to_cropland = "Bioenergy → cropland",
  other_to_cropland = "Other land → cropland"
)
food_signal_colours <- c(
  cropland_to_forest = "#1B9E77", cropland_to_bioenergy = "#D95F02",
  cropland_to_other = "#666666", forest_to_cropland = "#80CDC1",
  bioenergy_to_cropland = "#FDB863", other_to_cropland = "#377EB8"
)
food_signal_sign <- c(
  cropland_to_forest = -1, cropland_to_bioenergy = -1,
  cropland_to_other = -1, forest_to_cropland = 1,
  bioenergy_to_cropland = 1, other_to_cropland = 1
)

food_signal_summary <- lapply(names(food_signals), function(signal) {
  layer <- food_signals[[signal]]
  tibble(
    signal, signal_label = food_signal_labels[[signal]],
    direction = if_else(food_signal_sign[[signal]] < 0, "Cropland loss", "Cropland gain"),
    area_mha = global(layer * pixel_area, "sum", na.rm = TRUE)[[1]],
    signed_area_mha = food_signal_sign[[signal]] * area_mha
  )
}) %>% bind_rows()
write_csv(
  food_signal_summary,
  file.path(source_data_root, "supplementary_bidirectional_cropland_transition_summary.csv")
)

food_signal_pp <- mask(food_signals * 100, land_mask)
food_signal_limits <- vapply(names(food_signals), function(signal) {
  x <- values(food_signal_pp[[signal]], mat = FALSE)
  x <- x[is.finite(x) & x > 0]
  if (length(x) == 0L) 1 else unname(quantile(x, .98, na.rm = TRUE))
}, numeric(1))

food_signal_values <- as.data.frame(food_signal_pp, xy = TRUE, na.rm = FALSE)
food_signal_matrix <- as.matrix(food_signal_values[, names(food_signals)])
food_signal_matrix[!is.finite(food_signal_matrix)] <- 0
dominant_index <- max.col(food_signal_matrix, ties.method = "first")
dominant_value <- apply(food_signal_matrix, 1, max)
dominant_signal <- names(food_signals)[dominant_index]
material_transition <- dominant_value > .1

transition_gradient <- function(signal, value) {
  palette <- grDevices::colorRampPalette(
    c("#FAFAFA", food_signal_colours[[signal]])
  )(101)
  scaled <- pmin(value / food_signal_limits[[signal]], 1)
  palette[pmax(1L, pmin(101L, floor(sqrt(scaled) * 100) + 1L))]
}
dominant_fill <- rep(NA_character_, nrow(food_signal_values))
for (signal in names(food_signals)) {
  selected <- material_transition & dominant_signal == signal
  dominant_fill[selected] <- transition_gradient(signal, dominant_value[selected])
}
fig3_map_data <- food_signal_values %>%
  transmute(
    x, y, dominant_signal = if_else(material_transition, dominant_signal, NA_character_),
    dominant_change_pp = if_else(material_transition, dominant_value, NA_real_),
    fill_colour = dominant_fill
  ) %>%
  filter(!is.na(fill_colour))
write_csv(
  fig3_map_data %>% select(x, y, dominant_signal, dominant_change_pp),
  file.path(source_data_root, "supplementary_bidirectional_cropland_transition_grid.csv.gz")
)

p3a <- ggplot(fig3_map_data, aes(x, y, fill = fill_colour)) +
  geom_tile(width = .5, height = .5) +
  geom_sf(
    data = world_sf, fill = NA, colour = "grey25", linewidth = .12,
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  coord_world_no_antarctica() +
  labs(title = "Dominant cropland transition: MITI_PROT - MITI", tag = "a") +
  theme_map(7.8) + small_map_tag_theme

fig3_legend_data <- expand_grid(
  signal = factor(names(food_signals), levels = rev(names(food_signals))),
  intensity = seq(0, 1, length.out = 101)
) %>%
  mutate(
    signal_id = as.character(signal),
    fill_colour = mapply(
      function(s, z) transition_gradient(s, z * food_signal_limits[[s]]),
      signal_id, intensity, USE.NAMES = FALSE
    ),
    signal_label = factor(
      unname(food_signal_labels[signal_id]),
      levels = unname(food_signal_labels[rev(names(food_signals))])
    )
  )
p3legend <- ggplot(
  fig3_legend_data, aes(intensity, signal_label, fill = fill_colour)
) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = c(0, 1), labels = c("0", "Class-specific 98th percentile"),
    expand = c(0, 0)
  ) +
  labs(x = "Grid-cell area-share change", y = NULL) +
  theme_pub(7.0) +
  theme(
    panel.grid = element_blank(), axis.title.x = element_text(face = "bold"),
    plot.margin = margin(0, 65, 0, 65)
  )

fig3_net <- sum(food_signal_summary$signed_area_mha)

p3b <- ggplot(
  food_signal_summary,
  aes(x = factor("MITI_PROT − MITI"), y = signed_area_mha,
      fill = factor(signal, levels = names(food_signals)))
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .56) +
  geom_point(
    data = tibble(comparison = factor("MITI_PROT − MITI"), net_change_mha = fig3_net),
    aes(comparison, net_change_mha),
    inherit.aes = FALSE, shape = 18, size = 2.2
  ) +
  scale_fill_manual(values = food_signal_colours, labels = food_signal_labels, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(title = "Global cropland reallocation", tag = "b", x = NULL,
       y = "Cropland change (million ha)") +
  theme_pub(8.0) + small_map_tag_theme +
  theme(
    legend.position = "bottom", panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 6.5)
  ) +
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))

figure_s2 <- p3a / p3legend / p3b +
  plot_layout(heights = c(1.5, .72, 1.05))
ggsave(
  file.path(supplementary_root, "supplementary_bidirectional_cropland_transitions.png"),
  figure_s2, width = 7.2, height = 6.8, units = "in", dpi = 600,
  bg = "white", limitsize = FALSE
)
ggsave(
  file.path(supplementary_root, "supplementary_bidirectional_cropland_transitions.pdf"),
  figure_s2, width = 7.2, height = 6.8, units = "in", device = cairo_pdf,
  bg = "white", limitsize = FALSE
)

# Main Figure 3 retains the original cropland-centred four-signal logic.
main_food_signals <- c(
  food_signals[["cropland_to_forest"]],
  food_signals[["cropland_to_bioenergy"]],
  food_signals[["cropland_to_other"]],
  food_gain
)
names(main_food_signals) <- c(
  "cropland_to_forest", "cropland_to_bioenergy",
  "other_cropland_decline", "cropland_expansion"
)
main_food_labels <- c(
  cropland_to_forest = "Cropland → planted forest",
  cropland_to_bioenergy = "Cropland → bioenergy",
  other_cropland_decline = "Cropland → Other",
  cropland_expansion = "Newly expanded cropland"
)
main_food_colours <- c(
  cropland_to_forest = "#1B9E77", cropland_to_bioenergy = "#D95F02",
  other_cropland_decline = "#666666", cropland_expansion = "#377EB8"
)
main_food_sign <- c(
  cropland_to_forest = -1, cropland_to_bioenergy = -1,
  other_cropland_decline = -1, cropland_expansion = 1
)
main_food_summary <- lapply(names(main_food_signals), function(signal) {
  area_mha <- global(
    main_food_signals[[signal]] * pixel_area, "sum", na.rm = TRUE
  )[[1]]
  tibble(
    signal, signal_label = main_food_labels[[signal]], area_mha,
    signed_area_mha = main_food_sign[[signal]] * area_mha
  )
}) %>% bind_rows()

main_food_pp <- mask(main_food_signals * 100, land_mask)
main_food_positive_values <- values(main_food_pp, mat = TRUE)
main_food_positive_values <- main_food_positive_values[
  is.finite(main_food_positive_values) & main_food_positive_values > 0
]
main_food_display_limit <- if (length(main_food_positive_values) == 0L) {
  1
} else {
  unname(quantile(main_food_positive_values, .98, na.rm = TRUE))
}
main_food_summary <- main_food_summary %>%
  mutate(display_limit_pp = main_food_display_limit)
write_csv(
  main_food_summary,
  file.path(source_data_root, "figure_3_food_transition_summary.csv")
)
main_food_values <- as.data.frame(main_food_pp, xy = TRUE, na.rm = FALSE)
main_food_matrix <- as.matrix(main_food_values[, names(main_food_signals)])
main_food_matrix[!is.finite(main_food_matrix)] <- 0
main_dominant_index <- max.col(main_food_matrix, ties.method = "first")
main_dominant_value <- apply(main_food_matrix, 1, max)
main_dominant_signal <- names(main_food_signals)[main_dominant_index]
main_material <- main_dominant_value > .1
stable_cropland_colour <- "#F2E3C6"
stable_cropland <- mask(
  ifel(
    (
      land_grids[["011"]][["food"]] >= .01 |
        land_grids[["111"]][["food"]] >= .01
    ) & abs(food_delta * 100) <= .1,
    1, NA
  ),
  land_mask
)
stable_cropland_data <- to_map_data(stable_cropland, "stable") %>%
  filter(stable == 1) %>%
  transmute(
    x, y, dominant_signal = "stable_cropland",
    dominant_change_pp = 0, fill_colour = stable_cropland_colour
  )

main_transition_gradient <- function(signal, value) {
  palette <- grDevices::colorRampPalette(
    c("#FAFAFA", main_food_colours[[signal]])
  )(101)
  scaled <- pmin(value / main_food_display_limit, 1)
  palette[pmax(1L, pmin(101L, floor(sqrt(scaled) * 100) + 1L))]
}
main_dominant_fill <- rep(NA_character_, nrow(main_food_values))
for (signal in names(main_food_signals)) {
  selected <- main_material & main_dominant_signal == signal
  main_dominant_fill[selected] <- main_transition_gradient(
    signal, main_dominant_value[selected]
  )
}
fig3_main_map_data <- main_food_values %>%
  transmute(
    x, y,
    dominant_signal = if_else(main_material, main_dominant_signal, NA_character_),
    dominant_change_pp = if_else(main_material, main_dominant_value, NA_real_),
    fill_colour = main_dominant_fill
  ) %>%
  filter(!is.na(fill_colour))
write_csv(
  bind_rows(
    fig3_main_map_data %>% select(x, y, dominant_signal, dominant_change_pp),
    stable_cropland_data %>% select(x, y, dominant_signal, dominant_change_pp)
  ),
  file.path(source_data_root, "figure_3_food_transition_grid.csv.gz")
)

p3_main_a <- ggplot() +
  geom_tile(
    data = stable_cropland_data, aes(x, y),
    width = .5, height = .5, fill = stable_cropland_colour
  ) +
  geom_tile(
    data = fig3_main_map_data, aes(x, y, fill = fill_colour),
    width = .5, height = .5
  ) +
  geom_sf(
    data = world_sf, fill = NA, colour = "grey25", linewidth = .12,
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  coord_world_no_antarctica() +
  labs(title = "Cropland transition: MITI_PROT - MITI", tag = "a") +
  theme_map(7.8) + small_map_tag_theme

main_legend_signals <- c(names(main_food_signals), "stable_cropland")
main_legend_labels <- c(
  main_food_labels, stable_cropland = "Stable cropland (≤0.1 pp)"
)
fig3_main_legend_data <- expand_grid(
  signal = factor(main_legend_signals, levels = rev(main_legend_signals)),
  change_pp = seq(0, main_food_display_limit, length.out = 101)
) %>%
  mutate(
    signal_id = as.character(signal),
    fill_colour = mapply(
      function(signal, value) {
        if (signal == "stable_cropland") stable_cropland_colour else
          main_transition_gradient(signal, value)
      },
      signal_id, change_pp, USE.NAMES = FALSE
    ),
    signal_label = factor(
      unname(main_legend_labels[signal_id]),
      levels = unname(main_legend_labels[rev(main_legend_signals)])
    )
  )
p3_main_legend <- ggplot(
  fig3_main_legend_data, aes(change_pp, signal_label, fill = fill_colour)
) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = pretty(c(0, main_food_display_limit), n = 3),
    labels = label_number(accuracy = 1), expand = c(0, 0)
  ) +
  labs(
    x = "Grid-cell land-share change (percentage points)",
    y = NULL
  ) +
  theme_pub(5.3) +
  theme(
    panel.grid = element_blank(), axis.title.x = element_text(face = "bold", size = 5.4),
    axis.text = element_text(size = 4.9),
    plot.margin = margin(0, 4, 0, 4)
  )

main_food_net <- sum(main_food_summary$signed_area_mha)
p3_main_b <- ggplot(
  main_food_summary,
  aes(x = factor("MITI_PROT − MITI"), y = signed_area_mha,
      fill = factor(signal, levels = names(main_food_signals)))
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .54) +
  geom_point(
    data = tibble(comparison = factor("MITI_PROT − MITI"), net_change_mha = main_food_net),
    aes(comparison, net_change_mha), inherit.aes = FALSE,
    shape = 18, size = 2.1
  ) +
  scale_fill_manual(values = main_food_colours, guide = "none") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(tag = "b", x = NULL, y = "Cropland change (million ha)") +
  theme_pub(7.8) + small_map_tag_theme +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 5.7)
  )

p3_main_legend_compact <- (plot_spacer() | p3_main_legend | plot_spacer()) +
  plot_layout(widths = c(.20, .60, .20))
p3_main_left <- p3_main_a / p3_main_legend_compact +
  plot_layout(heights = c(1.95, .45))
figure_3 <- (p3_main_left | p3_main_b) +
  plot_layout(widths = c(4.55, .85))
save_figure(figure_3, 3, width = 7.2, height = 4.0)

# AgMIP market data used in Figures 4 and 5 -------------------------------

read_agmip_aggregate <- function(year = target_year) {
  agmip_file <- file.path(data_dir, "ALPHA", "AGMIP_all_scenario.gdx")
  requested_regions <- c("World", names(r10_labels))
  requested_variables <- c("AREA", "PROD", "CONS", "EXPO", "IMPO", "NETT", "CALI")
  requested_units <- c("1000 ha", "million kcal", "USD", "kcal/cap/d")
  request <- list(
    name = "AGMIP_agg", form = "full",
    uels = list(
      unname(states), requested_regions, requested_variables, "CRP",
      requested_units, as.character(year)
    )
  )
  array <- rgdx(agmip_file, request, squeeze = FALSE, useDomInfo = TRUE)
  raw <- as.data.frame.table(
    array$val, responseName = "value", stringsAsFactors = FALSE
  )
  names(raw) <- c(
    "scenario", "region", "variable", "item", "source_unit", "year", "value"
  )
  raw <- raw %>%
    mutate(
      state = state_from_scenario(scenario), value = as.numeric(value),
      year = as.integer(year)
    )

  energy <- raw %>%
    filter(
      variable %in% c("PROD", "CONS", "EXPO", "IMPO", "NETT"),
      source_unit == "million kcal"
    ) %>%
    transmute(
      region, state,
      indicator = recode(
        variable, PROD = "Production", CONS = "Consumption",
        EXPO = "Exports", IMPO = "Imports", NETT = "Net exports"
      ),
      unit = "PJ/yr", value = value * 4.184e-6
    )

  area <- raw %>%
    filter(variable == "AREA", source_unit == "1000 ha") %>%
    transmute(region, state, indicator = "Area", unit = "1000 ha", value)

  food_intake <- raw %>%
    filter(variable == "CALI", source_unit == "kcal/cap/d") %>%
    transmute(
      region, state, indicator = "Food intake",
      unit = "kcal/cap/day", value
    )

  producer_price <- raw %>%
    filter(variable == "PROD", source_unit %in% c("million kcal", "USD")) %>%
    select(region, state, source_unit, value) %>%
    pivot_wider(names_from = source_unit, values_from = value) %>%
    filter(`million kcal` > 1e-10) %>%
    transmute(
      region, state, indicator = "Producer price", unit = "USD/GJ",
      value = USD / (`million kcal` * 4.184)
    )

  bind_rows(energy, area, food_intake, producer_price)
}

decompose_area_yield <- function(market, comparisons) {
  wide <- market %>%
    filter(indicator %in% c("Area", "Production")) %>%
    select(region, state, indicator, value) %>%
    pivot_wider(
      names_from = c(indicator, state), values_from = value,
      names_glue = "{indicator}_{state}"
    )

  lapply(seq_len(nrow(comparisons)), function(i) {
    from <- comparisons$from[[i]]
    to <- comparisons$to[[i]]
    wide %>%
      transmute(
        region,
        comparison = comparisons$comparison[[i]],
        comparison_label = comparisons$comparison_label[[i]],
        area_from = .data[[paste0("Area_", from)]],
        area_to = .data[[paste0("Area_", to)]],
        production_from = .data[[paste0("Production_", from)]],
        production_to = .data[[paste0("Production_", to)]]
      ) %>%
      filter(
        area_from > 1e-12, area_to > 1e-12,
        production_from > 1e-12,
        if_all(
          c("area_from", "area_to", "production_from", "production_to"),
          is.finite
        )
      ) %>%
      mutate(
        yield_from = 1000 * production_from / area_from,
        yield_to = 1000 * production_to / area_to,
        area_effect = (area_to - area_from) * (yield_from + yield_to) / 2000,
        yield_effect = (yield_to - yield_from) * (area_from + area_to) / 2000,
        total_change = production_to - production_from,
        area_effect_pp = 100 * area_effect / production_from,
        yield_effect_pp = 100 * yield_effect / production_from,
        total_change_pct = 100 * total_change / production_from,
        closure = total_change - area_effect - yield_effect
      )
  }) %>% bind_rows()
}

market_aggregate <- read_agmip_aggregate()

# Figure 4: yield and production mechanisms --------------------------------

alpha_file <- file.path(data_dir, "ALPHA", "all_scenario_World.gdx")
alpha_yield_raw <- rgdx.param(alpha_file, "yldBas_load") %>%
  filter(
    SCENARIO %in% states,
    as.integer(as.character(Y)) == target_year,
    c %in% food_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO), crop = as.character(c),
    basin = as.character(bas), country = as.character(cty),
    yield_t_ha = as.numeric(yldBas_load)
  )
alpha_area_raw <- rgdx.param(alpha_file, "qlBas_load") %>%
  filter(
    SCENARIO %in% states,
    as.integer(as.character(Y)) == target_year,
    c %in% food_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO), crop = as.character(c),
    basin = as.character(bas), country = as.character(cty),
    area_1000ha = as.numeric(qlBas_load)
  )
base_weights <- alpha_area_raw %>%
  filter(state == "000", area_1000ha > 0) %>%
  select(crop, basin, country, weight = area_1000ha)

basin_yield <- alpha_yield_raw %>%
  left_join(base_weights, by = c("crop", "basin", "country")) %>%
  filter(is.finite(weight), weight > 0) %>%
  group_by(state, basin, country) %>%
  summarise(value = weighted.mean(yield_t_ha, weight), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = value) %>%
  transmute(
    basin, country,
    miti_vs_bau_pct = 100 * (`011` / `000` - 1),
    miti_prot_vs_bau_pct = 100 * (`111` / `000` - 1),
    miti_prot_minus_miti_pct = 100 * (`111` / `011` - 1)
  )

fig4_map_data <- basin_boundaries %>%
  left_join(basin_yield, by = c("basin", "country"))
write_csv(
  st_drop_geometry(fig4_map_data),
  file.path(source_data_root, "figure_4ab_basin_yield.csv")
)

plot_basin_change <- function(
  data, variable, title, tag, high = "#2166AC",
  legend_title = "Basin-level yield change (%)"
) {
  values <- data[[variable]]
  limit <- symmetric_limit(values)
  ggplot() +
    geom_sf(data = basin_boundaries, fill = "grey94", colour = NA) +
    geom_sf(data = data, aes(fill = .data[[variable]]), colour = NA) +
    geom_sf(data = world_sf, fill = NA, colour = "grey35", linewidth = .10) +
    scale_fill_gradient2(
      low = "#B2182B", mid = "#F7F7F7", high = high,
      midpoint = 0, limits = c(-limit, limit), oob = squish,
      name = legend_title
    ) +
    coord_world_no_antarctica() +
    labs(title = title, tag = tag) +
    theme_map(7.4) + small_map_tag_theme
}

p4a <- plot_basin_change(fig4_map_data, "miti_prot_vs_bau_pct",
                         "MITI_PROT - BAU", "a")
p4b <- plot_basin_change(fig4_map_data, "miti_prot_minus_miti_pct",
                         "MITI_PROT - MITI", "b") +
  theme(plot.margin = margin(10, 10, 10, 10))

decomposition_comparisons <- tribble(
  ~comparison, ~from, ~to, ~comparison_label,
  "miti_vs_bau", "000", "011", "MITI - BAU",
  "miti_prot_vs_bau", "000", "111", "MITI_PROT - BAU",
  "miti_prot_minus_miti", "011", "111", "MITI_PROT - MITI"
)
decomposition <- decompose_area_yield(
  market_aggregate, decomposition_comparisons
)
if (max(abs(decomposition$closure), na.rm = TRUE) > 1e-7) {
  stop("Area-yield decomposition did not close.")
}
write_csv(decomposition, file.path(source_data_root, "figure_4cd_production_decomposition.csv"))

component_colours <- c(
  "Harvested area" = "#D9A400", "Effective yield" = "#2166AC"
)
world_decomposition <- decomposition %>%
  filter(
    region == "World",
    comparison %in% c("miti_vs_bau", "miti_prot_vs_bau")
  ) %>%
  mutate(
    comparison_label = factor(
      comparison_label,
      levels = decomposition_comparisons$comparison_label[1:2]
    )
  )
world_decomposition_long <- world_decomposition %>%
  select(comparison_label, area_effect_pp, yield_effect_pp) %>%
  pivot_longer(
    c(area_effect_pp, yield_effect_pp),
    names_to = "component", values_to = "contribution_pp"
  ) %>%
  mutate(component = recode(
    component, area_effect_pp = "Harvested area",
    yield_effect_pp = "Effective yield"
  ))

p4c <- ggplot(
  world_decomposition_long,
  aes(comparison_label, contribution_pp, fill = component)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .65) +
  geom_point(
    data = world_decomposition,
    aes(comparison_label, total_change_pct, shape = "Total production change"),
    inherit.aes = FALSE, colour = "black", size = 2.1
  ) +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(tag = "c", x = NULL, y = "Contribution (% of baseline production)") +
  theme_pub(7.7) + tag_theme +
  theme(
    axis.text.x = element_text(angle = 32, hjust = 1, size = 6.5),
    legend.position = "bottom", legend.box = "horizontal",
    legend.text = element_text(size = 6.1),
    panel.grid.major.x = element_blank()
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  )

r10_decomposition <- decomposition %>%
  filter(
    region %in% names(r10_labels),
    comparison == "miti_prot_minus_miti"
  ) %>%
  mutate(region_label = factor(
    unname(r10_labels[region]), levels = rev(unname(r10_labels))
  ))
r10_decomposition_long <- r10_decomposition %>%
  select(region_label, area_effect_pp, yield_effect_pp) %>%
  pivot_longer(
    c(area_effect_pp, yield_effect_pp),
    names_to = "component", values_to = "contribution_pp"
  ) %>%
  mutate(component = recode(
    component, area_effect_pp = "Harvested area",
    yield_effect_pp = "Effective yield"
  ))

p4d <- ggplot(
  r10_decomposition_long,
  aes(region_label, contribution_pp, fill = component)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .65) +
  geom_point(
    data = r10_decomposition,
    aes(region_label, total_change_pct, shape = "Total production change"),
    inherit.aes = FALSE, colour = "black", size = 1.9
  ) +
  coord_flip() +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(title = "MITI_PROT - MITI", tag = "d", x = NULL,
       y = "Contribution (% of baseline production)") +
  theme_pub(7.7) + tag_theme +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.text = element_text(size = 6.1),
    panel.grid.major.y = element_blank()
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  )

p4_bottom <- (p4c | p4d) + plot_layout(widths = c(.9, 1.1))
figure_4 <- (p4a | p4b) / p4_bottom +
  plot_layout(heights = c(1.06, .94))
save_figure(figure_4, 4, width = 7.2, height = 6.2)

# Figure 5: agricultural-market effects and policy contributions -----------

shapley_three <- function(wide) {
  wide %>%
    mutate(
      baseline = `000`, final = `111`, total = `111` - `000`,
      protection = (`100` - `000`) / 3 +
        ((`110` - `010`) + (`101` - `001`)) / 6 +
        (`111` - `011`) / 3,
      planted_forest = (`010` - `000`) / 3 +
        ((`110` - `100`) + (`011` - `001`)) / 6 +
        (`111` - `101`) / 3,
      beccs = (`001` - `000`) / 3 +
        ((`101` - `100`) + (`011` - `010`)) / 6 +
        (`111` - `110`) / 3,
      residual = total - protection - planted_forest - beccs
    )
}

market_shapley <- market_aggregate %>%
  select(region, indicator, unit, state, value) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  filter(if_all(all_of(names(states)), is.finite)) %>%
  shapley_three() %>%
  mutate(
    protection_pct = if_else(abs(baseline) > 1e-10,
                             100 * protection / baseline, NA_real_),
    planted_forest_pct = if_else(abs(baseline) > 1e-10,
                                100 * planted_forest / baseline, NA_real_),
    beccs_pct = if_else(abs(baseline) > 1e-10,
                        100 * beccs / baseline, NA_real_),
    total_pct = if_else(abs(baseline) > 1e-10,
                        100 * total / baseline, NA_real_)
  )
if (max(abs(market_shapley$residual), na.rm = TRUE) > 1e-7) {
  stop("Market Shapley decomposition did not close.")
}
write_csv(market_shapley, file.path(source_data_root, "supplementary_market_shapley.csv"))

policy_labels <- c(
  protection_pct = "Protection", planted_forest_pct = "Planted forest",
  beccs_pct = "BECCS"
)
world_market <- market_shapley %>% filter(region == "World")
world_main <- world_market %>%
  filter(indicator %in% c("Production", "Producer price", "Food intake")) %>%
  mutate(indicator = factor(
    indicator, levels = c("Production", "Producer price", "Food intake")
  ))
world_main_long <- world_main %>%
  select(indicator, all_of(names(policy_labels))) %>%
  pivot_longer(-indicator, names_to = "policy", values_to = "contribution") %>%
  mutate(policy = factor(unname(policy_labels[policy]), levels = unname(policy_labels)))

p5a <- ggplot(world_main_long, aes(indicator, contribution, fill = policy)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .64) +
  geom_point(
    data = world_main, aes(indicator, total_pct),
    inherit.aes = FALSE, shape = 18, size = 2.2
  ) +
  scale_fill_manual(values = policy_colours, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(title = "World", tag = "a", x = NULL,
       y = "Contribution (% of no-policy value)") +
  theme_pub() + tag_theme +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom", panel.grid.major.x = element_blank()
  )

world_trade <- world_market %>%
  filter(indicator %in% c("Imports", "Exports")) %>%
  mutate(indicator = factor(indicator, levels = c("Imports", "Exports")))
world_trade_long <- world_trade %>%
  select(indicator, protection, planted_forest, beccs) %>%
  pivot_longer(-indicator, names_to = "policy", values_to = "contribution") %>%
  mutate(policy = factor(
    recode(policy, protection = "Protection", planted_forest = "Planted forest", beccs = "BECCS"),
    levels = names(policy_colours)
  ))

p5b <- ggplot(world_trade_long, aes(indicator, contribution, fill = policy)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .64) +
  geom_point(
    data = world_trade, aes(indicator, total),
    inherit.aes = FALSE, shape = 18, size = 2.2
  ) +
  scale_fill_manual(values = policy_colours, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(title = "World trade", tag = "b", x = NULL,
       y = "Contribution (PJ yr-1)") +
  theme_pub() + tag_theme +
  theme(
    legend.position = "none", panel.grid.major.x = element_blank()
  )

plot_r10_market <- function(indicator_name, title, tag) {
  data <- market_shapley %>%
    filter(region %in% names(r10_labels), indicator == indicator_name) %>%
    mutate(region_label = factor(
      unname(r10_labels[region]), levels = rev(unname(r10_labels))
    ))
  long <- data %>%
    select(region_label, all_of(names(policy_labels))) %>%
    pivot_longer(-region_label, names_to = "policy", values_to = "contribution") %>%
    mutate(policy = factor(
      unname(policy_labels[policy]), levels = unname(policy_labels)
    ))
  ggplot(long, aes(region_label, contribution, fill = policy)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
    geom_col(width = .64) +
    geom_point(
      data = data, aes(region_label, total_pct),
      inherit.aes = FALSE, shape = 18, size = 1.8
    ) +
    coord_flip() +
    scale_fill_manual(values = policy_colours, name = NULL) +
    scale_y_continuous(labels = label_number(accuracy = .1)) +
    labs(title = title, tag = tag, x = NULL,
         y = "Contribution (% of no-policy value)") +
    theme_pub(7.8) + tag_theme +
    theme(
      legend.position = "none", panel.grid.major.y = element_blank()
    )
}

p5c <- plot_r10_market("Production", "Regional production", "c")
p5d <- plot_r10_market("Producer price", "Regional producer price", "d")
p5e <- plot_r10_market("Food intake", "Regional food intake", "e")

figure_s1 <- (p5a | p5b) / p5c / p5d / p5e +
  plot_layout(heights = c(.85, 1, 1, 1))
ggsave(
  file.path(supplementary_root, "supplementary_shapley.png"), figure_s1,
  width = 7.2, height = 10.0, units = "in", dpi = 600,
  bg = "white", limitsize = FALSE
)
ggsave(
  file.path(supplementary_root, "supplementary_shapley.pdf"), figure_s1,
  width = 7.2, height = 10.0, units = "in", device = cairo_pdf,
  bg = "white", limitsize = FALSE
)

# Main Figure 5 uses only BAU, MITI and MITI_PROT. -------------------------

market_direct <- market_aggregate %>%
  filter(state %in% c("000", "011", "111")) %>%
  select(region, indicator, unit, state, value) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  filter(if_all(all_of(c("000", "011", "111")), is.finite)) %>%
  mutate(
    miti_pct = if_else(abs(`000`) > 1e-10, 100 * (`011` / `000` - 1), NA_real_),
    miti_prot_pct = if_else(abs(`000`) > 1e-10, 100 * (`111` / `000` - 1), NA_real_),
    miti_prot_minus_miti_pct = if_else(
      abs(`011`) > 1e-10, 100 * (`111` / `011` - 1), NA_real_
    ),
    miti_change = `011` - `000`,
    miti_prot_change = `111` - `000`,
    miti_prot_minus_miti_change = `111` - `011`
  )
write_csv(
  market_direct,
  file.path(source_data_root, "figure_5_market_scenario_comparison.csv")
)

main_scenario_colours <- c(
  MITI = "#0072B2",
  MITI_PROT = "#D55E00"
)
world_direct <- market_direct %>% filter(region == "World")
world_direct_main <- world_direct %>%
  filter(indicator %in% c("Production", "Producer price", "Food intake")) %>%
  mutate(indicator = factor(
    indicator, levels = c("Production", "Producer price", "Food intake")
  ))
world_direct_long <- world_direct_main %>%
  select(indicator, miti_pct, miti_prot_pct) %>%
  pivot_longer(-indicator, names_to = "scenario", values_to = "change_pct") %>%
  mutate(scenario = factor(
    recode(scenario, miti_pct = "MITI", miti_prot_pct = "MITI_PROT"),
    levels = c("MITI", "MITI_PROT")
  ))

p5_main_a <- ggplot(
  world_direct_long, aes(indicator, change_pct, fill = scenario)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(position = position_dodge(width = .7), width = .62) +
  scale_fill_manual(values = main_scenario_colours, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(title = "World", tag = "a", x = NULL,
       y = "Change from BAU (%)") +
  theme_pub() + tag_theme +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom", panel.grid.major.x = element_blank()
  )

plot_r10_direct <- function(indicator_name, title, tag) {
  data <- market_direct %>%
    filter(region %in% names(r10_labels), indicator == indicator_name) %>%
    mutate(region_label = factor(
      unname(r10_labels[region]), levels = rev(unname(r10_labels))
    ))
  long <- data %>%
    select(region_label, miti_pct, miti_prot_pct) %>%
    pivot_longer(-region_label, names_to = "scenario", values_to = "change_pct") %>%
    mutate(scenario = factor(
      recode(scenario, miti_pct = "MITI", miti_prot_pct = "MITI_PROT"),
      levels = c("MITI", "MITI_PROT")
    ))
  ggplot(long, aes(region_label, change_pct, fill = scenario)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
    geom_col(position = position_dodge(width = .72), width = .64) +
    coord_flip() +
    scale_fill_manual(values = main_scenario_colours, name = NULL) +
    scale_y_continuous(labels = label_number(accuracy = .1)) +
    labs(title = title, tag = tag, x = NULL, y = "Change from BAU (%)") +
    theme_pub(7.8) + tag_theme +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
}

p5_main_c <- plot_r10_direct("Production", "Production", "b")
p5_main_d <- plot_r10_direct("Producer price", "Producer price", "c")
p5_main_e <- plot_r10_direct("Food intake", "Food intake", "d")

figure_5 <- p5_main_a / p5_main_c / p5_main_d / p5_main_e +
  plot_layout(heights = c(.8, 1, 1, 1))
save_figure(figure_5, 5, width = 7.2, height = 8.8)

# Figure 6: basin land pressure and crop-production relocation ------------

# Crop production is converted with the same country-commodity calorie
# coefficients used by ALPHA's AgMIP output. Missing coefficients are retained
# as zero caloric production, matching the model output convention.
calorie_coefficients <- read_csv(
  file.path(data_dir, "ALPHA", "calCoef.csv"), show_col_types = FALSE
)

basin_production_raw <- rgdx.param(alpha_file, "qsBas_load") %>%
  filter(
    SCENARIO %in% states[c("011", "111")],
    as.integer(as.character(Y)) == target_year,
    c %in% food_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO), commodity = as.character(c),
    basin = as.character(bas), country = as.character(cty),
    production_1000t = as.numeric(qsBas_load)
  ) %>%
  left_join(calorie_coefficients, by = c("commodity", "country"))

figure6_calorie_coverage <- basin_production_raw %>%
  group_by(state) %>%
  summarise(
    total_production_1000t = sum(production_1000t, na.rm = TRUE),
    production_with_calorie_coefficient_1000t = sum(
      production_1000t[is.finite(million_kcal_per_1000t)], na.rm = TRUE
    ),
    mass_coverage_pct = 100 * production_with_calorie_coefficient_1000t /
      total_production_1000t,
    .groups = "drop"
  )
write_csv(
  figure6_calorie_coverage,
  file.path(source_data_root, "figure_6_calorie_coefficient_coverage.csv")
)

basin_production <- basin_production_raw %>%
  filter(is.finite(million_kcal_per_1000t)) %>%
  group_by(state, basin, country) %>%
  summarise(
    crop_production_pj = sum(
      production_1000t * million_kcal_per_1000t * 4.184e-6,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

read_basin_area <- function(symbol, value_column, commodities = NULL) {
  data <- rgdx.param(alpha_file, symbol) %>%
    filter(
      SCENARIO %in% states[c("011", "111")],
      as.integer(as.character(Y)) == target_year
    )
  if (!is.null(commodities)) {
    data <- data %>% filter(c %in% commodities)
  }
  data %>%
    transmute(
      state = state_from_scenario(SCENARIO), basin = as.character(bas),
      country = as.character(cty), value = as.numeric(.data[[value_column]])
    ) %>%
    group_by(state, basin, country) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

basin_total <- read_basin_area("qlTot_load", "qlTot_load") %>%
  rename(total_land_1000ha = value)
basin_protected <- rgdx.param(alpha_file, "qlProt_load") %>%
  filter(
    SCENARIO %in% states[c("011", "111")],
    as.integer(as.character(Y)) == target_year
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO), basin = as.character(bas),
    country = as.character(cty), value = as.numeric(qlProt_load)
  ) %>%
  group_by(state, basin, country) %>%
  summarise(protected_1000ha = sum(value, na.rm = TRUE), .groups = "drop")
basin_planted <- read_basin_area("qlAfr_load", "qlAfr_load") %>%
  rename(planted_forest_1000ha = value)
basin_bioenergy <- read_basin_area("qlBas_load", "qlBas_load", "ecr") %>%
  rename(bioenergy_1000ha = value)
basin_food_area <- read_basin_area(
  "qlBas_load", "qlBas_load", food_crop_codes
) %>%
  rename(food_harvested_area_1000ha = value)

basin_state_data <- basin_total %>%
  left_join(basin_protected, by = c("state", "basin", "country")) %>%
  left_join(basin_planted, by = c("state", "basin", "country")) %>%
  left_join(basin_bioenergy, by = c("state", "basin", "country")) %>%
  left_join(basin_food_area, by = c("state", "basin", "country")) %>%
  left_join(basin_production, by = c("state", "basin", "country")) %>%
  mutate(across(
    c(
      protected_1000ha, planted_forest_1000ha, bioenergy_1000ha,
      food_harvested_area_1000ha, crop_production_pj
    ),
    ~ replace_na(.x, 0)
  ))

basin_metrics <- basin_state_data %>%
  pivot_wider(
    names_from = state,
    values_from = c(
      total_land_1000ha, protected_1000ha, planted_forest_1000ha,
      bioenergy_1000ha, food_harvested_area_1000ha, crop_production_pj
    ),
    names_glue = "{.value}_{state}"
  ) %>%
  filter(total_land_1000ha_011 > 0) %>%
  mutate(
    protection_change_pp = 100 *
      (protected_1000ha_111 - protected_1000ha_011) / total_land_1000ha_011,
    planted_forest_change_pp = 100 *
      (planted_forest_1000ha_111 - planted_forest_1000ha_011) /
      total_land_1000ha_011,
    bioenergy_change_pp = 100 *
      (bioenergy_1000ha_111 - bioenergy_1000ha_011) / total_land_1000ha_011,
    cdr_land_change_pp = planted_forest_change_pp + bioenergy_change_pp,
    food_area_change_pp = 100 *
      (food_harvested_area_1000ha_111 - food_harvested_area_1000ha_011) /
      total_land_1000ha_011,
    crop_production_change_pj = crop_production_pj_111 - crop_production_pj_011,
    crop_production_change_pct = if_else(
      crop_production_pj_011 > 1e-6,
      100 * crop_production_change_pj / crop_production_pj_011,
      NA_real_
    ),
    response_class = case_when(
      abs(cdr_land_change_pp) <= .01 & abs(food_area_change_pp) <= .01 ~
        "Small change",
      cdr_land_change_pp > 0 & food_area_change_pp < 0 ~
        "CDR expansion, food-area contraction",
      cdr_land_change_pp < 0 & food_area_change_pp > 0 ~
        "CDR contraction, food-area expansion",
      cdr_land_change_pp >= 0 & food_area_change_pp >= 0 ~ "Both expand",
      TRUE ~ "Both contract"
    )
  )

response_levels <- c(
  "CDR expansion, food-area contraction",
  "CDR contraction, food-area expansion", "Both expand", "Both contract",
  "Small change"
)
response_colours <- c(
  "CDR expansion, food-area contraction" = "#D73027",
  "CDR contraction, food-area expansion" = "#4575B4",
  "Both expand" = "#7B3294", "Both contract" = "#8C8C8C",
  "Small change" = "#F1E8D2"
)
basin_metrics <- basin_metrics %>%
  mutate(response_class = factor(response_class, levels = response_levels))
write_csv(
  basin_metrics,
  file.path(source_data_root, "figure_6_basin_land_production.csv")
)

basin_response_sf <- basin_boundaries %>%
  left_join(
    basin_metrics,
    by = c("basin", "country")
  )
p6a <- ggplot() +
  geom_sf(data = basin_boundaries, fill = "grey96", colour = "white", linewidth = .03) +
  geom_sf(
    data = basin_response_sf, aes(fill = response_class),
    colour = "white", linewidth = .03
  ) +
  geom_sf(data = world_sf, fill = NA, colour = "grey30", linewidth = .12) +
  scale_fill_manual(values = response_colours, drop = FALSE, name = NULL) +
  coord_world_no_antarctica() +
  labs(tag = "a") +
  theme_map(7.2) + small_map_tag_theme +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 5.5),
    legend.key.width = unit(3.2, "mm")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

basin_plot_data <- basin_metrics %>%
  filter(crop_production_pj_011 > .01, is.finite(crop_production_change_pct))
basin_prod_limit <- quantile(
  abs(basin_plot_data$crop_production_change_pct), .99, na.rm = TRUE
)
p6b <- ggplot(
  basin_plot_data,
  aes(cdr_land_change_pp, food_area_change_pp, colour = response_class)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .25) +
  geom_point(aes(size = crop_production_pj_011), alpha = .34, stroke = 0) +
  scale_colour_manual(values = response_colours, guide = "none") +
  scale_size_area(max_size = 2.4, guide = "none") +
  scale_x_continuous(
    trans = pseudo_log_trans(sigma = .01),
    breaks = c(-50, -10, -1, 0, 1, 10, 50),
    labels = label_number(accuracy = .01)
  ) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = .01),
    breaks = c(-10, -1, -.1, 0, .1, 1, 10),
    labels = label_number(accuracy = .01)
  ) +
  labs(tag = "b", x = "Planted forest + bioenergy change\n(% of basin land)",
       y = "Food harvested-area change\n(% of basin land)") +
  theme_pub(7.2) + tag_theme +
  theme(axis.text.x = element_text(size = 5.8))

p6c <- ggplot(
  basin_plot_data,
  aes(food_area_change_pp, crop_production_change_pct, colour = response_class)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .25) +
  geom_point(aes(size = crop_production_pj_011), alpha = .34, stroke = 0) +
  scale_colour_manual(values = response_colours, guide = "none") +
  scale_size_area(max_size = 2.4, guide = "none") +
  scale_x_continuous(
    trans = pseudo_log_trans(sigma = .01),
    breaks = c(-10, -1, -.1, 0, .1, 1, 10),
    labels = label_number(accuracy = .01)
  ) +
  coord_cartesian(ylim = c(-basin_prod_limit, basin_prod_limit)) +
  labs(tag = "c", x = "Food harvested-area change\n(% of basin land)",
       y = "Crop production change (%)") +
  theme_pub(7.2) + tag_theme +
  theme(axis.text.x = element_text(size = 5.8))

basin_pressure_long <- basin_plot_data %>%
  select(
    basin, country, crop_production_pj_011, crop_production_change_pct,
    protection_change_pp, planted_forest_change_pp, bioenergy_change_pp
  ) %>%
  pivot_longer(
    c(protection_change_pp, planted_forest_change_pp, bioenergy_change_pp),
    names_to = "pressure", values_to = "land_change_pp"
  ) %>%
  mutate(pressure = factor(
    recode(
      pressure, protection_change_pp = "Additional protection",
      planted_forest_change_pp = "Planted forest",
      bioenergy_change_pp = "Bioenergy"
    ),
    levels = c("Additional protection", "Planted forest", "Bioenergy")
  ))
basin_pressure_stats <- basin_pressure_long %>%
  group_by(pressure) %>%
  summarise(
    rho = cor(land_change_pp, crop_production_change_pct,
              method = "spearman", use = "complete.obs"),
    n = sum(is.finite(land_change_pp) & is.finite(crop_production_change_pct)),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("rho = %.2f; n = %d", rho, n))
p6d <- ggplot(
  basin_pressure_long,
  aes(land_change_pp, crop_production_change_pct, weight = crop_production_pj_011)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .25) +
  geom_point(colour = "#4D4D4D", alpha = .18, size = .55) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = "#0072B2", linewidth = .5) +
  geom_text(
    data = basin_pressure_stats,
    aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
    hjust = -.08, vjust = 1.25, size = 2.1
  ) +
  facet_wrap(vars(pressure), scales = "free_x", nrow = 1) +
  scale_x_continuous(
    breaks = breaks_pretty(n = 3), labels = label_number(accuracy = .1)
  ) +
  coord_cartesian(ylim = c(-basin_prod_limit, basin_prod_limit)) +
  labs(tag = "d", x = "Change (% of basin land)",
       y = "Crop production change (%)") +
  theme_pub(7.0) + tag_theme +
  theme(
    panel.spacing.x = unit(5, "mm"),
    axis.text.x = element_text(size = 5.3)
  )

figure_6 <- p6a / (p6b | p6c) / p6d +
  plot_layout(heights = c(1.2, 1, .95))
save_figure(figure_6, 6, width = 7.2, height = 8.0)

# Figure 7: country production shocks and trade adjustment ----------------

agmip_file <- file.path(data_dir, "ALPHA", "AGMIP_all_scenario.gdx")
country_r10 <- rgdx.set(agmip_file, "cty_reg_map") %>%
  transmute(country = as.character(cty), r10 = as.character(reg)) %>%
  filter(grepl("^R10", r10)) %>%
  distinct(country, .keep_all = TRUE)
country_codes <- country_r10$country

country_market_request <- list(
  name = "AGMIP", form = "full",
  uels = list(
    unname(states[c("011", "111")]), country_codes,
    c("PROD", "CONS", "IMPO", "EXPO", "CALI", "POPT"),
    c("CRP", "TOT"), c("million kcal", "USD", "kcal/cap/d", "million"),
    as.character(target_year)
  )
)
country_market_array <- rgdx(
  agmip_file, country_market_request, squeeze = FALSE, useDomInfo = TRUE
)
country_market_raw <- as.data.frame.table(
  country_market_array$val, responseName = "value", stringsAsFactors = FALSE
)
names(country_market_raw) <- c(
  "scenario", "country", "variable", "item", "source_unit", "year", "value"
)
country_market_raw <- country_market_raw %>%
  mutate(
    state = state_from_scenario(scenario), value = as.numeric(value),
    year = as.integer(year)
  )

country_energy <- country_market_raw %>%
  filter(
    item == "CRP", variable %in% c("PROD", "CONS", "IMPO", "EXPO"),
    source_unit == "million kcal"
  ) %>%
  transmute(country, state, variable, value_pj = value * 4.184e-6) %>%
  pivot_wider(
    names_from = c(variable, state), values_from = value_pj,
    names_glue = "{variable}_{state}"
  )
country_price <- country_market_raw %>%
  filter(item == "CRP", variable == "PROD",
         source_unit %in% c("million kcal", "USD")) %>%
  select(country, state, source_unit, value) %>%
  pivot_wider(names_from = source_unit, values_from = value) %>%
  filter(`million kcal` > 1e-10) %>%
  transmute(country, state, producer_price_usd_gj = USD / (`million kcal` * 4.184)) %>%
  pivot_wider(
    names_from = state, values_from = producer_price_usd_gj,
    names_glue = "producer_price_{state}"
  )
country_intake <- country_market_raw %>%
  filter(item == "CRP", variable == "CALI", source_unit == "kcal/cap/d") %>%
  transmute(country, state, food_intake = value) %>%
  pivot_wider(
    names_from = state, values_from = food_intake,
    names_glue = "food_intake_{state}"
  )
country_population <- country_market_raw %>%
  filter(item == "TOT", variable == "POPT", source_unit == "million", state == "011") %>%
  transmute(country, population_million = value)

country_market <- country_energy %>%
  left_join(country_price, by = "country") %>%
  left_join(country_intake, by = "country") %>%
  left_join(country_population, by = "country") %>%
  left_join(country_r10, by = "country") %>%
  filter(CONS_011 > 1e-8, PROD_011 > 1e-8) %>%
  mutate(
    production_change_pct = 100 * (PROD_111 / PROD_011 - 1),
    consumption_change_pct = 100 * (CONS_111 / CONS_011 - 1),
    food_intake_change_pct = 100 * (food_intake_111 / food_intake_011 - 1),
    producer_price_change_pct = 100 *
      (producer_price_111 / producer_price_011 - 1),
    domestic_production_contribution_pp = 100 * (PROD_111 - PROD_011) / CONS_011,
    net_import_contribution_pp = 100 *
      ((IMPO_111 - IMPO_011) - (EXPO_111 - EXPO_011)) / CONS_011,
    consumption_change_contribution_pp = 100 * (CONS_111 - CONS_011) / CONS_011,
    balance_residual_pp = consumption_change_contribution_pp -
      domestic_production_contribution_pp - net_import_contribution_pp,
    baseline_import_dependence_pct = 100 * IMPO_011 / CONS_011,
    baseline_trade_openness_pct = 100 * (IMPO_011 + EXPO_011) / CONS_011,
    trade_offset_ratio = if_else(
      abs(domestic_production_contribution_pp) > .1,
      -net_import_contribution_pp / domestic_production_contribution_pp,
      NA_real_
    ),
    trade_offset_opposing_pp = -sign(domestic_production_contribution_pp) *
      net_import_contribution_pp
  )
write_csv(
  country_market,
  file.path(source_data_root, "figure_7_country_trade_buffering.csv")
)

r10_colours <- setNames(
  hue_pal(h = c(15, 375), c = 90, l = 55)(length(r10_labels)),
  names(r10_labels)
)
country_label_data <- country_market %>%
  slice_max(abs(domestic_production_contribution_pp), n = 12, with_ties = FALSE)
p7a <- ggplot(
  country_market,
  aes(domestic_production_contribution_pp, net_import_contribution_pp,
      colour = r10)
) +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed",
              colour = "grey45", linewidth = .35) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = .25) +
  geom_point(aes(size = population_million), alpha = .62, stroke = 0) +
  geom_text_repel(
    data = country_label_data, aes(label = country),
    size = 2.0, max.overlaps = Inf, min.segment.length = 0,
    box.padding = .2, show.legend = FALSE
  ) +
  scale_colour_manual(values = r10_colours, guide = "none") +
  scale_size_area(max_size = 3.5, guide = "none") +
  scale_x_continuous(
    trans = pseudo_log_trans(sigma = 1), breaks = sparse_pretty_breaks,
    labels = label_number(accuracy = 1)
  ) +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 1), breaks = sparse_pretty_breaks,
    labels = label_number(accuracy = 1)
  ) +
  labs(tag = "a", x = "Domestic production contribution\n(% of baseline consumption)",
       y = "Net-import adjustment\n(% of baseline consumption)") +
  theme_pub(7.2) + tag_theme

p7b <- ggplot(
  country_market,
  aes(production_change_pct, consumption_change_pct,
      colour = baseline_import_dependence_pct)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey45", linewidth = .35) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = .25) +
  geom_point(aes(size = population_million), alpha = .65, stroke = 0) +
  scale_colour_viridis_c(option = "C", end = .88, name = "Import dependence (%)") +
  scale_size_area(max_size = 3.5, guide = "none") +
  labs(tag = "b", x = "Crop production change (%)",
       y = "Crop consumption change (%)") +
  theme_pub(7.2) + tag_theme +
  theme(legend.position = "bottom", legend.key.width = unit(9, "mm"))

country_bar_order <- country_market %>%
  slice_max(abs(domestic_production_contribution_pp), n = 18, with_ties = FALSE) %>%
  arrange(domestic_production_contribution_pp) %>%
  pull(country)
country_bar_data <- country_market %>%
  filter(country %in% country_bar_order) %>%
  mutate(country = factor(country, levels = country_bar_order))
country_bar_long <- country_bar_data %>%
  select(
    country, domestic_production_contribution_pp, net_import_contribution_pp,
    balance_residual_pp
  ) %>%
  pivot_longer(-country, names_to = "component", values_to = "contribution_pp") %>%
  mutate(component = factor(
    recode(
      component,
      domestic_production_contribution_pp = "Domestic production",
      net_import_contribution_pp = "Net imports",
      balance_residual_pp = "Other balance terms"
    ),
    levels = c("Domestic production", "Net imports", "Other balance terms")
  ))
p7c <- ggplot(country_bar_long, aes(country, contribution_pp, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .3) +
  geom_col(width = .68) +
  geom_point(
    data = country_bar_data,
    aes(country, consumption_change_contribution_pp),
    inherit.aes = FALSE, shape = 18, size = 1.8
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Domestic production" = "#0072B2", "Net imports" = "#E69F00",
      "Other balance terms" = "#999999"
    ),
    name = NULL
  ) +
  labs(tag = "c", x = NULL, y = "Contribution (% of baseline consumption)") +
  theme_pub(7.0) + tag_theme +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())

country_offset_data <- country_market %>%
  filter(abs(domestic_production_contribution_pp) >= .5,
         is.finite(trade_offset_opposing_pp))
country_offset_labels <- country_offset_data %>%
  slice_max(abs(trade_offset_opposing_pp), n = 10, with_ties = FALSE)
p7d <- ggplot(
  country_offset_data,
  aes(baseline_import_dependence_pct, trade_offset_opposing_pp, colour = r10)
) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .25) +
  geom_point(aes(size = population_million), alpha = .62, stroke = 0) +
  geom_text_repel(
    data = country_offset_labels, aes(label = country), size = 2.0,
    max.overlaps = Inf, min.segment.length = 0, show.legend = FALSE
  ) +
  scale_colour_manual(values = r10_colours, guide = "none") +
  scale_size_area(max_size = 3.5, guide = "none") +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = .5), breaks = sparse_pretty_breaks,
    labels = label_number(accuracy = 1)
  ) +
  labs(tag = "d", x = "Baseline import dependence (%)",
       y = "Trade adjustment opposing production shock\n(% of baseline consumption)") +
  theme_pub(7.2) + tag_theme

figure_7 <- (p7a | p7b) / (p7c | p7d) +
  plot_layout(heights = c(1, 1.12))
save_figure(figure_7, 7, width = 7.2, height = 7.2)

# Figure 8: country land-policy exposure and agricultural responses --------

country_land_variables <- c(
  Lan_Cov = "total_land",
  Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int = "protected_land",
  Lan_Cov_Frs_Aff = "planted_forest",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "bioenergy",
  Lan_Cov_Cro_Non_Ene_Cro = "food_cropland"
)
country_land <- merged_iamc %>%
  filter(
    geography %in% country_codes, state %in% c("011", "111"),
    year == target_year, variable %in% names(country_land_variables)
  ) %>%
  transmute(
    country = geography, state,
    land_key = unname(country_land_variables[variable]), area_mha = value
  ) %>%
  group_by(country, state, land_key) %>%
  summarise(area_mha = sum(area_mha, na.rm = TRUE), .groups = "drop") %>%
  complete(
    country = country_codes, state = c("011", "111"),
    land_key = unname(country_land_variables), fill = list(area_mha = 0)
  ) %>%
  pivot_wider(
    names_from = c(land_key, state), values_from = area_mha,
    names_glue = "{land_key}_{state}"
  ) %>%
  filter(total_land_011 > 0) %>%
  mutate(
    protection_added_pp = 100 *
      (protected_land_111 - protected_land_011) / total_land_011,
    planted_forest_change_pp = 100 *
      (planted_forest_111 - planted_forest_011) / total_land_011,
    bioenergy_change_pp = 100 *
      (bioenergy_111 - bioenergy_011) / total_land_011,
    food_cropland_change_pp = 100 *
      (food_cropland_111 - food_cropland_011) / total_land_011,
    cdr_land_change_pp = planted_forest_change_pp + bioenergy_change_pp
  ) %>%
  left_join(country_market, by = "country")
write_csv(
  country_land,
  file.path(source_data_root, "figure_8_country_land_market.csv")
)

country_land_rank <- country_land %>%
  rowwise() %>%
  mutate(land_response_score = max(
    abs(c(protection_added_pp, planted_forest_change_pp,
          bioenergy_change_pp)), na.rm = TRUE
  )) %>%
  ungroup() %>%
  slice_max(land_response_score, n = 25, with_ties = FALSE) %>%
  arrange(land_response_score) %>%
  mutate(country = factor(country, levels = country))
country_land_rank_long <- country_land_rank %>%
  select(country, protection_added_pp, planted_forest_change_pp,
         bioenergy_change_pp) %>%
  pivot_longer(-country, names_to = "land_response", values_to = "change_pp") %>%
  mutate(land_response = factor(
    recode(
      land_response,
      protection_added_pp = "Additional protection",
      planted_forest_change_pp = "Planted forest",
      bioenergy_change_pp = "Bioenergy"
    ),
    levels = c("Additional protection", "Planted forest", "Bioenergy")
  ))
p8a <- ggplot(
  country_land_rank_long,
  aes(change_pp, country, colour = land_response, shape = land_response)
) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_point(size = 1.8) +
  scale_colour_manual(
    values = c(
      "Additional protection" = "#7B3294", "Planted forest" = "#1B9E77",
      "Bioenergy" = "#E08214"
    ), name = NULL
  ) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  scale_x_continuous(
    breaks = breaks_pretty(n = 5), labels = label_number(accuracy = .1)
  ) +
  labs(tag = "a", x = "Change (% of national land)", y = NULL) +
  theme_pub(7.0) + tag_theme +
  theme(
    legend.position = "bottom", panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 6)
  )

country_pressure_long <- country_land %>%
  filter(is.finite(production_change_pct)) %>%
  select(
    country, r10, population_million, production_change_pct,
    protection_added_pp, planted_forest_change_pp, bioenergy_change_pp
  ) %>%
  pivot_longer(
    c(protection_added_pp, planted_forest_change_pp, bioenergy_change_pp),
    names_to = "land_response", values_to = "change_pp"
  ) %>%
  mutate(land_response = factor(
    recode(
      land_response,
      protection_added_pp = "Additional protection",
      planted_forest_change_pp = "Planted forest",
      bioenergy_change_pp = "Bioenergy"
    ),
    levels = c("Additional protection", "Planted forest", "Bioenergy")
  ))
country_pressure_labels <- country_pressure_long %>%
  group_by(land_response) %>%
  slice_max(abs(change_pp), n = 5, with_ties = FALSE) %>%
  ungroup()
p8b <- ggplot(
  country_pressure_long,
  aes(change_pp, production_change_pct, colour = r10)
) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey65", linewidth = .25) +
  geom_point(aes(size = population_million), alpha = .55, stroke = 0) +
  geom_smooth(
    aes(weight = population_million), method = "lm", formula = y ~ x,
    se = FALSE, colour = "black", linewidth = .45
  ) +
  geom_text_repel(
    data = country_pressure_labels, aes(label = country), size = 1.8,
    max.overlaps = Inf, min.segment.length = 0, show.legend = FALSE
  ) +
  facet_wrap(vars(land_response), scales = "free_x", nrow = 1) +
  scale_colour_manual(values = r10_colours, guide = "none") +
  scale_size_area(max_size = 3.2, guide = "none") +
  scale_x_continuous(
    breaks = breaks_pretty(n = 3), labels = label_number(accuracy = .1)
  ) +
  labs(tag = "b", x = "Change (% of national land)",
       y = "Crop production change (%)") +
  theme_pub(6.8) + tag_theme +
  theme(
    panel.spacing.x = unit(5, "mm"),
    axis.text.x = element_text(size = 5.2)
  )

model_predictors <- c(
  protection_added_pp = "Additional protection",
  planted_forest_change_pp = "Planted forest",
  bioenergy_change_pp = "Bioenergy",
  baseline_import_dependence_pct = "Import dependence"
)
model_outcomes <- c(
  production_change_pct = "Production",
  producer_price_change_pct = "Producer price",
  food_intake_change_pct = "Food intake",
  net_import_contribution_pp = "Net-import adjustment"
)
country_model_rows <- list()
for (outcome_name in names(model_outcomes)) {
  model_data <- country_land %>%
    select(
      all_of(c(outcome_name, names(model_predictors))), r10,
      weight = PROD_011
    ) %>%
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    mutate(
      outcome_z = as.numeric(scale(.data[[outcome_name]])),
      protection_z = as.numeric(scale(protection_added_pp)),
      planted_z = as.numeric(scale(planted_forest_change_pp)),
      bioenergy_z = as.numeric(scale(bioenergy_change_pp)),
      import_z = as.numeric(scale(baseline_import_dependence_pct)),
      model_weight = sqrt(pmax(weight, 1e-8))
    )
  fit <- lm(
    outcome_z ~ protection_z + planted_z + bioenergy_z + import_z + factor(r10),
    data = model_data, weights = model_weight
  )
  coefficient_table <- summary(fit)$coefficients
  predictor_rows <- c(
    protection_z = "protection_added_pp",
    planted_z = "planted_forest_change_pp",
    bioenergy_z = "bioenergy_change_pp",
    import_z = "baseline_import_dependence_pct"
  )
  selected_rows <- coefficient_table[names(predictor_rows), , drop = FALSE]
  country_model_rows[[outcome_name]] <- tibble(
    outcome = unname(model_outcomes[[outcome_name]]),
    predictor = unname(model_predictors[predictor_rows]),
    estimate = selected_rows[, "Estimate"],
    std_error = selected_rows[, "Std. Error"],
    lower = estimate - 1.96 * std_error,
    upper = estimate + 1.96 * std_error,
    n = nrow(model_data), r_squared = summary(fit)$r.squared
  )
}
country_model_coefficients <- bind_rows(country_model_rows) %>%
  mutate(
    outcome = factor(outcome, levels = unname(model_outcomes)),
    predictor = factor(predictor, levels = rev(unname(model_predictors)))
  )
write_csv(
  country_model_coefficients,
  file.path(source_data_root, "figure_8_country_descriptive_regressions.csv")
)
p8c <- ggplot(
  country_model_coefficients,
  aes(estimate, predictor, xmin = lower, xmax = upper)
) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .3) +
  geom_errorbar(orientation = "y", width = .18,
                linewidth = .35, colour = "grey35") +
  geom_point(size = 1.6, colour = "#0072B2") +
  facet_wrap(vars(outcome), ncol = 2) +
  labs(tag = "c", x = "Standardized coefficient", y = NULL) +
  theme_pub(6.8) + tag_theme +
  theme(panel.grid.major.y = element_blank())

correlation_pressures <- c(
  protection_added_pp = "Additional protection",
  planted_forest_change_pp = "Planted forest",
  bioenergy_change_pp = "Bioenergy",
  food_cropland_change_pp = "Food cropland"
)
correlation_outcomes <- c(
  production_change_pct = "Production",
  producer_price_change_pct = "Producer price",
  food_intake_change_pct = "Food intake",
  net_import_contribution_pp = "Net imports"
)
country_correlation_rows <- list()
row_id <- 0L
for (pressure_name in names(correlation_pressures)) {
  for (outcome_name in names(correlation_outcomes)) {
    selected <- country_land %>%
      select(x = all_of(pressure_name), y = all_of(outcome_name)) %>%
      filter(is.finite(x), is.finite(y))
    test <- cor.test(selected$x, selected$y, method = "spearman", exact = FALSE)
    row_id <- row_id + 1L
    country_correlation_rows[[row_id]] <- tibble(
      pressure = unname(correlation_pressures[[pressure_name]]),
      outcome = unname(correlation_outcomes[[outcome_name]]),
      rho = unname(test$estimate), p_value = test$p.value, n = nrow(selected)
    )
  }
}
country_correlations <- bind_rows(country_correlation_rows) %>%
  mutate(
    pressure = factor(pressure, levels = rev(unname(correlation_pressures))),
    outcome = factor(outcome, levels = unname(correlation_outcomes))
  )
write_csv(
  country_correlations,
  file.path(source_data_root, "figure_8_country_spearman_correlations.csv")
)
p8d <- ggplot(country_correlations, aes(outcome, pressure, fill = rho)) +
  geom_tile(colour = "white", linewidth = .35) +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 2.2) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
    limits = c(-1, 1), name = "Spearman rho"
  ) +
  labs(tag = "d", x = NULL, y = NULL) +
  theme_pub(6.8) + tag_theme +
  theme(
    panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom", legend.key.width = unit(10, "mm")
  )

figure_8 <- p8a / p8b / (p8c | p8d) +
  plot_layout(heights = c(1.3, .9, 1.15))
save_figure(figure_8, 8, width = 7.2, height = 10.2)

write_lines(capture.output(sessionInfo()), file.path(output_root, "session_info.txt"))
message("Wrote eight publication figures to: ", output_root)
