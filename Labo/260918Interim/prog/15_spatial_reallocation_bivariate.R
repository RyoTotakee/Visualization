suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
  library(tidyr)
})

source("prog/common.R")
init_gdx()
sf_use_s2(FALSE)

analysis_root <- file.path(output_dir, "15_spatial_reallocation_bivariate")
unlink(analysis_root, recursive = TRUE)
direct_root <- file.path(analysis_root, "01_direct_comparison")
baseline_root <- file.path(analysis_root, "02_baseline_response_bivariate")
protection_root <- file.path(analysis_root, "03_protection_response_bivariate")
data_root <- file.path(analysis_root, "04_data_and_matrices")
transition_root <- file.path(analysis_root, "05_cropland_transition_signals")
vegetation_root <- file.path(analysis_root, "06_avoided_vegetation_loss")
bio_transition_root <- file.path(analysis_root, "07_bioenergy_transition_signals")
forest_transition_root <- file.path(analysis_root, "08_planted_forest_transition_signals")
for (path in c(
  direct_root, baseline_root, protection_root, data_root,
  transition_root, vegetation_root, bio_transition_root,
  forest_transition_root
)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

map_theme <- theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9.2, colour = "grey30", margin = margin(b = 5)),
    plot.caption = element_text(size = 7.8, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(8, 8, 8, 8)
  )

states <- scenario_states("global2")
grid_years <- c(2005L, 2010L, seq(2020L, 2100L, by = 10L))
year_position <- match(target_year, grid_years)
land_classes <- c(
  food = "Food cropland",
  bio = "2G bioenergy cropland",
  planted_forest = "Planted forest"
)
land_class_index <- c(food = 1L, bio = 2L, planted_forest = 5L)
land_class_stubs <- c(
  food = "food_cropland", bio = "bioenergy_cropland",
  planted_forest = "planted_forest"
)

# TEMPORARY WORKAROUND: existing AIM-LUmap files double-count Rbasin and R17.
# Remove this factor after regenerating them with the corrected combine.gms.
grid_export_duplicate_factor <- 2

read_land_grid <- function(state) {
  path <- file.path(
    data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[[state]], ".nc")
  )
  all_layers <- rast(path, subds = "LC_area_share")
  positions <- (unname(land_class_index) - 1L) * length(grid_years) +
    year_position
  selected <- all_layers[[positions]] / grid_export_duplicate_factor
  names(selected) <- names(land_class_index)
  selected
}

grid_without_protection <- read_land_grid("011")
grid_with_protection <- read_land_grid("111")
reference_file <- file.path(
  data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[["011"]], ".nc")
)
pixel_area <- rast(reference_file, subds = "pixel_area")

# Use all 12 land-cover classes only to identify land cells. The source shares
# are retained as supplied, including documented values outside 0--1.
all_reference_layers <- rast(reference_file, subds = "LC_area_share")
mask_positions <- (0:11) * length(grid_years) + year_position
land_mask <- app(all_reference_layers[[mask_positions]], sum, na.rm = TRUE)
land_mask[land_mask <= 0] <- NA

world <- ne_countries(scale = "small", returnclass = "sf")

to_map_data <- function(x, value_name) {
  as.data.frame(mask(x, land_mask), xy = TRUE, na.rm = TRUE) %>%
    rename(!!value_name := 3)
}

plot_continuous_map <- function(data, value, title, subtitle, palette, limits,
                                legend_title) {
  p <- ggplot(data, aes(x, y, fill = .data[[value]])) +
    geom_tile(width = .5, height = .5) +
    geom_sf(
      data = world, fill = NA, colour = "grey25", linewidth = .12,
      inherit.aes = FALSE
    ) +
    coord_sf(expand = FALSE) +
    labs(title = title, subtitle = subtitle)
  if (length(palette) == 2L) {
    p <- p + scale_fill_gradient(
      low = palette[[1]], high = palette[[2]], limits = limits,
      oob = squish, name = legend_title
    )
  } else {
    p <- p + scale_fill_gradient2(
      low = palette[[1]], mid = palette[[2]], high = palette[[3]],
      midpoint = 0, limits = limits, oob = squish, name = legend_title
    )
  }
  p + map_theme
}

# 1. Direct map comparison: without protection, with protection, difference -

delta_layers <- grid_with_protection - grid_without_protection
names(delta_layers) <- names(land_classes)
writeRaster(
  delta_layers,
  file.path(data_root, paste0("grid_land_share_change_111_minus_011_", target_year, ".tif")),
  overwrite = TRUE
)

direct_summary <- list()
for (class_id in names(land_classes)) {
  from_share <- mask(grid_without_protection[[class_id]] * 100, land_mask)
  to_share <- mask(grid_with_protection[[class_id]] * 100, land_mask)
  delta_pp <- to_share - from_share

  pooled_values <- c(
    values(from_share, mat = FALSE), values(to_share, mat = FALSE)
  )
  pooled_values <- pooled_values[is.finite(pooled_values) & pooled_values >= 0]
  share_limit <- unname(quantile(pooled_values, .99, na.rm = TRUE))
  delta_values <- values(delta_pp, mat = FALSE)
  delta_values <- delta_values[is.finite(delta_values) & delta_values != 0]
  delta_limit <- unname(quantile(abs(delta_values), .98, na.rm = TRUE))

  from_data <- to_map_data(from_share, "value")
  to_data <- to_map_data(to_share, "value")
  delta_data <- to_map_data(delta_pp, "value")
  land_label <- unname(land_classes[[class_id]])

  p_from <- plot_continuous_map(
    from_data, "value", "Without protection", "Both CDR (011)",
    c("#f7fbff", "#08519c"), c(0, share_limit), "Land share\n(percentage points)"
  ) + theme(legend.position = "none")
  p_to <- plot_continuous_map(
    to_data, "value", "With protection", "Protection + both CDR (111)",
    c("#f7fbff", "#08519c"), c(0, share_limit), "Land share\n(percentage points)"
  )
  p_delta <- plot_continuous_map(
    delta_data, "value", "Grid difference", "111 minus 011",
    c("#b2182b", "#f7f7f7", "#2166ac"),
    c(-delta_limit, delta_limit), "Change\n(percentage points)"
  )

  direct_plot <- p_from + p_to + p_delta +
    plot_layout(widths = c(1, 1.06, 1.06)) +
    plot_annotation(
      title = paste0(land_label, ": protection changes where land is located"),
      subtitle = paste0(
        "The first two maps use the same colour scale; the third subtracts 011 from 111 in ",
        target_year
      ),
      caption = paste0(
        "AIM-LU 0.5-degree LC_area_share. Display limits use pooled 99th and difference 98th percentiles; ",
        "underlying raster values are not truncated."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 10.5, colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
      )
    )
  ggsave(
    file.path(
      direct_root,
      paste0(land_class_stubs[[class_id]], "_without_with_difference_", target_year, ".png")
    ),
    direct_plot, width = 16.5, height = 5.8, dpi = 300, bg = "white"
  )

  gained <- global(ifel(delta_pp > 0, delta_pp / 100, 0) * pixel_area,
                   "sum", na.rm = TRUE)[[1]]
  lost <- global(ifel(delta_pp < 0, -delta_pp / 100, 0) * pixel_area,
                 "sum", na.rm = TRUE)[[1]]
  direct_summary[[class_id]] <- tibble(
    class_id, land_class = land_label, target_year,
    display_share_limit_pp = share_limit,
    display_delta_limit_pp = delta_limit,
    area_gained_mha = gained, area_lost_mha = lost,
    net_change_mha = gained - lost,
    spatially_relocated_mha = min(gained, lost)
  )
}
direct_summary <- bind_rows(direct_summary)
write_csv(
  direct_summary,
  file.path(data_root, paste0("direct_grid_comparison_summary_", target_year, ".csv"))
)

# 2. Bivariate map: prior land intensity x response to protection -----------

# Rows correspond to decrease/stable/increase and columns to low/medium/high
# prior land share. Red identifies source-side contraction, blue destination-
# side expansion, and darkness shows how concentrated the land use was in 011.
bivariate_colours <- c(
  `1` = "#fee5d9", `2` = "#fcae91", `3` = "#cb181d",
  `4` = "#f7f7f7", `5` = "#e0e0e0", `6` = "#bdbdbd",
  `7` = "#deebf7", `8` = "#9ecae1", `9` = "#2171b5"
)
response_labels <- c("Decrease", "Stable", "Increase")
intensity_labels <- c("Low", "Medium", "High")
stable_threshold_pp <- 0.1

make_matrix_legend <- function(x_title) {
  legend_data <- expand_grid(
    intensity_id = 1:3, response_id = 1:3
  ) %>%
    mutate(
      code = factor(intensity_id + 3L * (response_id - 1L), levels = 1:9),
      intensity = factor(intensity_labels[intensity_id], levels = intensity_labels),
      response = factor(response_labels[response_id], levels = response_labels)
    )
  ggplot(legend_data, aes(intensity, response, fill = code)) +
    geom_tile(colour = "white", linewidth = .9) +
    scale_fill_manual(values = bivariate_colours, guide = "none") +
    coord_equal() +
    labs(x = x_title, y = "Land response after protection") +
    theme_minimal(base_size = 9.5) +
    theme(
      panel.grid = element_blank(), axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "grey20"),
      plot.margin = margin(2, 10, 4, 10)
    )
}

plot_bivariate_map <- function(code_layer, title, subtitle) {
  map_data <- to_map_data(code_layer, "code") %>%
    mutate(code = factor(round(code), levels = 1:9))
  ggplot(map_data, aes(x, y, fill = code)) +
    geom_tile(width = .5, height = .5) +
    geom_sf(
      data = world, fill = NA, colour = "grey20", linewidth = .12,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = bivariate_colours, guide = "none", na.value = "grey92") +
    coord_sf(expand = FALSE) +
    labs(title = title, subtitle = subtitle) +
    map_theme
}

baseline_code_layers <- NULL
baseline_summary <- list()
baseline_maps <- list()
for (class_id in names(land_classes)) {
  from_share <- mask(grid_without_protection[[class_id]] * 100, land_mask)
  delta_pp <- mask(
    (grid_with_protection[[class_id]] - grid_without_protection[[class_id]]) * 100,
    land_mask
  )
  positive_from <- values(from_share, mat = FALSE)
  positive_from <- positive_from[is.finite(positive_from) & positive_from > 0]
  intensity_breaks <- unname(quantile(positive_from, c(1 / 3, 2 / 3), na.rm = TRUE))

  intensity_class <- ifel(
    from_share <= intensity_breaks[[1]], 1,
    ifel(from_share <= intensity_breaks[[2]], 2, 3)
  )
  response_class <- ifel(
    delta_pp < -stable_threshold_pp, 1,
    ifel(delta_pp > stable_threshold_pp, 3, 2)
  )
  code_layer <- intensity_class + 3 * (response_class - 1)
  names(code_layer) <- class_id
  baseline_code_layers <- if (is.null(baseline_code_layers)) {
    code_layer
  } else {
    c(baseline_code_layers, code_layer)
  }

  land_label <- unname(land_classes[[class_id]])
  baseline_maps[[class_id]] <- plot_bivariate_map(
    code_layer,
    land_label,
    paste0("011 intensity x 111-011 response; stable is within +/-", stable_threshold_pp, " pp")
  )

  code_values <- values(code_layer, mat = FALSE)
  delta_values <- values(delta_pp, mat = FALSE)
  area_values <- values(pixel_area, mat = FALSE)
  valid <- is.finite(code_values) & is.finite(delta_values) & is.finite(area_values)
  baseline_summary[[class_id]] <- tibble(
    code = as.integer(code_values[valid]),
    delta_pp = delta_values[valid], pixel_area_mha = area_values[valid]
  ) %>%
    mutate(
      intensity_id = ((code - 1L) %% 3L) + 1L,
      response_id = ((code - 1L) %/% 3L) + 1L
    ) %>%
    group_by(intensity_id, response_id) %>%
    summarise(
      cell_count = n(), area_mha = sum(pixel_area_mha),
      land_change_mha = sum(delta_pp / 100 * pixel_area_mha),
      area_weighted_change_pp = weighted.mean(delta_pp, pixel_area_mha),
      .groups = "drop"
    ) %>%
    mutate(
      class_id, land_class = land_label,
      without_protection_intensity = intensity_labels[intensity_id],
      protection_response = response_labels[response_id],
      lower_intensity_break_pp = intensity_breaks[[1]],
      upper_intensity_break_pp = intensity_breaks[[2]],
      stable_threshold_pp
    )
}
names(baseline_code_layers) <- names(land_classes)
writeRaster(
  baseline_code_layers,
  file.path(
    data_root, paste0("baseline_intensity_response_bivariate_classes_", target_year, ".tif")
  ),
  datatype = "INT1U", overwrite = TRUE
)
baseline_summary <- bind_rows(baseline_summary)
write_csv(
  baseline_summary,
  file.path(data_root, paste0("baseline_response_bivariate_area_summary_", target_year, ".csv"))
)

baseline_legend <- make_matrix_legend("Land share without protection (011)")
baseline_figure <- wrap_plots(
  c(baseline_maps, list(baseline_legend)), ncol = 1,
  heights = c(1, 1, 1, .75)
) +
  plot_annotation(
    title = "Protection relocates land from source cells to destination cells",
    subtitle = "Red cells contract after protection; blue cells expand; colour darkness shows the land-use intensity before protection",
    caption = paste0(
      "AIM-LU 0.5-degree comparison, 111 minus 011 in ", target_year,
      ". Intensity breaks are class-specific terciles of positive 011 shares."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10.5, colour = "grey30"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
    )
  )
ggsave(
  file.path(
    baseline_root, paste0("land_reallocation_bivariate_maps_", target_year, ".png")
  ),
  baseline_figure, width = 12, height = 14.5, dpi = 300, bg = "white"
)

# Quantitative 3 x 3 matrices use the same classes as the bivariate map.
baseline_matrix_plot <- baseline_summary %>%
  mutate(
    without_protection_intensity = factor(
      without_protection_intensity, levels = intensity_labels
    ),
    protection_response = factor(protection_response, levels = response_labels),
    code = factor(intensity_id + 3L * (response_id - 1L), levels = 1:9)
  ) %>%
  ggplot(aes(without_protection_intensity, protection_response, fill = code)) +
  geom_tile(colour = "white", linewidth = .8) +
  geom_text(aes(label = sprintf("%.0f Mha\n%+.1f Mha", area_mha, land_change_mha)),
            size = 3) +
  facet_wrap(vars(land_class), nrow = 1) +
  scale_fill_manual(values = bivariate_colours, guide = "none") +
  coord_equal() +
  labs(
    title = "Area represented by each spatial reallocation class",
    subtitle = "Top number is cell area; bottom number is the net change in the named land use within those cells",
    x = "Land share without protection (011)",
    y = "Land response after protection (111-011)"
  ) + theme_paper() +
  theme(strip.text = element_text(face = "bold"), panel.grid = element_blank())
ggsave(
  file.path(data_root, paste0("baseline_response_area_matrices_", target_year, ".png")),
  baseline_matrix_plot, width = 13.5, height = 5.4, dpi = 300, bg = "white"
)

# 3. Bivariate map: basin protection pressure x grid land response ----------

# The supplied AIM-LU NetCDF contains land use and pixel area, but no protected-
# area raster. protPlum supplies primary forest, secondary forest and grassland
# protected shares on ALPHA basins. Their unweighted mean is used as a pressure
# proxy and rasterised to the AIM-LU grid; it is not direct cell overlap.
plum_states <- scenario_states("global1")
basin_protection <- rgdx.param(
  file.path(data_dir, "ALPHA", "plum_data.gdx"), "protPlum"
) %>%
  filter(
    sce_plum %in% plum_states[c("011", "111")],
    as.integer(as.character(yr)) == target_year,
    lu_prot %in% c("primf", "secdf", "gra")
  ) %>%
  transmute(
    state = state_from_scenario(as.character(sce_plum), "global1"),
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
    basin, country,
    protection_increase_pp = 100 * (`111` - `011`)
  )
write_csv(
  basin_protection,
  file.path(data_root, paste0("basin_protection_pressure_111_minus_011_", target_year, ".csv"))
)

basin_boundaries <- st_read(
  file.path(data_dir, "Basin_Boundary", "Basin_Boundary.shp"), quiet = TRUE
) %>%
  transmute(country = as.character(ISO_A3), basin = as.character(Basin))
basin_protection_sf <- basin_boundaries %>%
  left_join(basin_protection, by = c("country", "basin"))

protection_values <- basin_protection$protection_increase_pp
protection_values <- protection_values[
  is.finite(protection_values) & protection_values > 0
]
protection_limit <- unname(quantile(protection_values, .98, na.rm = TRUE))
protection_plot <- ggplot(basin_protection_sf) +
  geom_sf(aes(fill = protection_increase_pp), colour = NA) +
  geom_sf(data = world, fill = NA, colour = "grey25", linewidth = .12) +
  scale_fill_gradient(
    low = "#f2f0f7", high = "#54278f", limits = c(0, protection_limit),
    oob = squish, na.value = "grey92", name = "Protection pressure\n(percentage points)"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "A. Additional protection pressure",
    subtitle = "Basin proxy: mean increase for primary forest, secondary forest and grassland"
  ) + map_theme

protection_raster <- rasterize(
  vect(basin_protection_sf), pixel_area,
  field = "protection_increase_pp", background = NA, touches = TRUE
)
protection_breaks <- unname(quantile(
  values(protection_raster, mat = FALSE), c(1 / 3, 2 / 3),
  na.rm = TRUE
))
pressure_class <- ifel(
  protection_raster <= protection_breaks[[1]], 1,
  ifel(protection_raster <= protection_breaks[[2]], 2, 3)
)

protection_code_layers <- NULL
protection_response_summary <- list()
protection_response_maps <- list()
panel_letters <- c(food = "D", bio = "C", planted_forest = "B")
for (class_id in names(land_classes)) {
  delta_pp <- mask(
    (grid_with_protection[[class_id]] - grid_without_protection[[class_id]]) * 100,
    land_mask
  )
  response_class <- ifel(
    delta_pp < -stable_threshold_pp, 1,
    ifel(delta_pp > stable_threshold_pp, 3, 2)
  )
  code_layer <- mask(pressure_class + 3 * (response_class - 1), land_mask)
  names(code_layer) <- class_id
  protection_code_layers <- if (is.null(protection_code_layers)) {
    code_layer
  } else {
    c(protection_code_layers, code_layer)
  }
  land_label <- unname(land_classes[[class_id]])
  protection_response_maps[[class_id]] <- plot_bivariate_map(
    code_layer,
    paste0(panel_letters[[class_id]], ". ", land_label, " response"),
    "Red = contraction; blue = expansion; darkness = protection pressure"
  )

  code_values <- values(code_layer, mat = FALSE)
  delta_values <- values(delta_pp, mat = FALSE)
  area_values <- values(pixel_area, mat = FALSE)
  valid <- is.finite(code_values) & is.finite(delta_values) & is.finite(area_values)
  protection_response_summary[[class_id]] <- tibble(
    code = as.integer(code_values[valid]), delta_pp = delta_values[valid],
    pixel_area_mha = area_values[valid]
  ) %>%
    mutate(
      pressure_id = ((code - 1L) %% 3L) + 1L,
      response_id = ((code - 1L) %/% 3L) + 1L
    ) %>%
    group_by(pressure_id, response_id) %>%
    summarise(
      cell_count = n(), area_mha = sum(pixel_area_mha),
      land_change_mha = sum(delta_pp / 100 * pixel_area_mha),
      area_weighted_change_pp = weighted.mean(delta_pp, pixel_area_mha),
      .groups = "drop"
    ) %>%
    mutate(
      class_id, land_class = land_label,
      protection_pressure = intensity_labels[pressure_id],
      protection_response = response_labels[response_id],
      lower_pressure_break_pp = protection_breaks[[1]],
      upper_pressure_break_pp = protection_breaks[[2]], stable_threshold_pp
    )
}
names(protection_code_layers) <- names(land_classes)
writeRaster(
  protection_code_layers,
  file.path(
    data_root, paste0("protection_pressure_response_bivariate_classes_", target_year, ".tif")
  ),
  datatype = "INT1U", overwrite = TRUE
)
protection_response_summary <- bind_rows(protection_response_summary)
write_csv(
  protection_response_summary,
  file.path(data_root, paste0("protection_response_bivariate_area_summary_", target_year, ".csv"))
)

protection_legend <- make_matrix_legend("Additional basin protection pressure")
story_figure <- wrap_plots(
  A = protection_plot,
  B = protection_response_maps[["planted_forest"]],
  C = protection_response_maps[["bio"]],
  D = protection_response_maps[["food"]],
  E = protection_legend,
  design = "
  AB
  CD
  EE
  ",
  heights = c(1, 1, .7)
) +
  plot_annotation(
    title = "Where protection redirects afforestation, bioenergy and food cropland",
    subtitle = "High-pressure contractions identify likely source areas; low-pressure expansions identify possible relocation destinations",
    caption = paste0(
      "Land response is the AIM-LU grid difference 111-011 in ", target_year,
      ". Protection pressure is a rasterised ALPHA-basin proxy from global1 protPlum, not a direct 0.5-degree protection mask."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10.5, colour = "grey30"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
    )
  )
ggsave(
  file.path(
    protection_root, paste0("figure2_candidate_protection_land_reallocation_", target_year, ".png")
  ),
  story_figure, width = 15, height = 11.8, dpi = 300, bg = "white"
)

protection_matrix_plot <- protection_response_summary %>%
  mutate(
    protection_pressure = factor(protection_pressure, levels = intensity_labels),
    protection_response = factor(protection_response, levels = response_labels),
    code = factor(pressure_id + 3L * (response_id - 1L), levels = 1:9)
  ) %>%
  ggplot(aes(protection_pressure, protection_response, fill = code)) +
  geom_tile(colour = "white", linewidth = .8) +
  geom_text(aes(label = sprintf("%.0f Mha\n%+.1f Mha", area_mha, land_change_mha)),
            size = 3) +
  facet_wrap(vars(land_class), nrow = 1) +
  scale_fill_manual(values = bivariate_colours, guide = "none") +
  coord_equal() +
  labs(
    title = "Land response by basin protection-pressure class",
    subtitle = "Top number is cell area; bottom number is net land-use change within those cells",
    x = "Additional basin protection pressure",
    y = "Land response after protection (111-011)"
  ) + theme_paper() +
  theme(strip.text = element_text(face = "bold"), panel.grid = element_blank())
ggsave(
  file.path(data_root, paste0("protection_response_area_matrices_", target_year, ".png")),
  protection_matrix_plot, width = 13.5, height = 5.4, dpi = 300, bg = "white"
)

# 4. Cropland-transition signals ------------------------------------------

# Fractional land shares do not store a cell-level transition matrix. The
# following signals match food-cropland loss to simultaneous planted-forest
# and bioenergy gains in the same cell. The matched food loss is allocated in
# proportion to the two gains, so the two candidate transitions never double
# count the same cropland loss.
food_delta <- grid_with_protection[["food"]] - grid_without_protection[["food"]]
bio_delta <- grid_with_protection[["bio"]] - grid_without_protection[["bio"]]
forest_delta <- grid_with_protection[["planted_forest"]] -
  grid_without_protection[["planted_forest"]]
food_loss <- ifel(food_delta < 0, -food_delta, 0)
food_gain <- ifel(food_delta > 0, food_delta, 0)
bio_gain <- ifel(bio_delta > 0, bio_delta, 0)
forest_gain <- ifel(forest_delta > 0, forest_delta, 0)
candidate_gain <- bio_gain + forest_gain
matched_food_loss <- ifel(
  candidate_gain > 0,
  ifel(food_loss < candidate_gain, food_loss, candidate_gain),
  0
)
unmatched_food_loss <- food_loss - matched_food_loss

transition_signals <- c(
  ifel(candidate_gain > 0, matched_food_loss * forest_gain / candidate_gain, 0),
  ifel(candidate_gain > 0, matched_food_loss * bio_gain / candidate_gain, 0),
  food_gain,
  unmatched_food_loss
)
names(transition_signals) <- c(
  "food_to_planted_forest", "food_to_bioenergy", "food_cropland_expansion",
  "other_food_cropland_decline"
)
transition_signals <- mask(transition_signals, land_mask)
writeRaster(
  transition_signals,
  file.path(data_root, paste0("cropland_transition_signals_", target_year, ".tif")),
  overwrite = TRUE
)

transition_labels <- c(
  food_to_planted_forest = "Cropland loss paired with planted-forest gain",
  food_to_bioenergy = "Cropland loss paired with bioenergy gain",
  food_cropland_expansion = "New or expanded food cropland",
  other_food_cropland_decline = "Other cropland decline (unmatched to forest/bio gain)"
)
transition_colours <- c(
  food_to_planted_forest = "#1B9E77",
  food_to_bioenergy = "#D95F02",
  food_cropland_expansion = "#377EB8",
  other_food_cropland_decline = "#666666"
)
transition_limits <- vapply(names(transition_labels), function(signal) {
  v <- values(transition_signals[[signal]] * 100, mat = FALSE)
  v <- v[is.finite(v) & v > 0]
  if (length(v) == 0L) 1 else unname(quantile(v, .98, na.rm = TRUE))
}, numeric(1))

stable_food <- mask(
  ifel(
    app(c(
      grid_without_protection[["food"]], grid_with_protection[["food"]]
    ), max, na.rm = TRUE) > .01 &
      app(transition_signals, max, na.rm = TRUE) <= .001,
    1, NA
  ),
  land_mask
)
stable_food_data <- to_map_data(stable_food, "stable")

transition_panels <- list()
for (signal in names(transition_labels)) {
  signal_data <- to_map_data(transition_signals[[signal]] * 100, "signal_pp") %>%
    filter(signal_pp > 0)
  transition_panels[[signal]] <- ggplot() +
    geom_tile(
      data = stable_food_data, aes(x, y), width = .5, height = .5,
      fill = "grey94"
    ) +
    geom_tile(
      data = signal_data, aes(x, y, fill = signal_pp), width = .5, height = .5
    ) +
    geom_sf(
      data = world, fill = NA, colour = "grey20", linewidth = .12,
      inherit.aes = FALSE
    ) +
    scale_fill_gradient(
      low = "#f7f7f7", high = transition_colours[[signal]],
      limits = c(0, transition_limits[[signal]]), oob = squish,
      name = "Matched share\n(percentage points)"
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = unname(transition_labels[[signal]]),
      subtitle = "Pale grey marks materially stable food-cropland cells"
    ) + map_theme
}

transition_panel_figure <- wrap_plots(transition_panels, ncol = 1) +
  plot_annotation(
    title = "Four spatial signals reveal where food cropland is redirected or lost",
    subtitle = "Protection is added to both CDR options: 111 minus 011",
    caption = paste0(
      "Same-cell transition signals in ", target_year,
      ". Cropland loss is proportionally matched to simultaneous planted-forest and bioenergy gains. The grey residual is not proof of direct protection or natural-land conversion."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10.5, colour = "grey30"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
    )
  )
ggsave(
  file.path(
    transition_root, paste0("cropland_transition_signals_four_panels_", target_year, ".png")
  ),
  transition_panel_figure, width = 12, height = 16, dpi = 300, bg = "white"
)

# A single categorical-gradient map uses the strongest of the four signals in
# each cell. Hue identifies the signal and saturation shows its magnitude.
transition_values <- as.data.frame(
  transition_signals * 100, xy = TRUE, na.rm = FALSE
)
signal_columns <- names(transition_labels)
signal_matrix <- as.matrix(transition_values[, signal_columns])
signal_matrix[!is.finite(signal_matrix)] <- 0
dominant_index <- max.col(signal_matrix, ties.method = "first")
dominant_value <- apply(signal_matrix, 1, max)
dominant_signal <- signal_columns[dominant_index]
material <- dominant_value > stable_threshold_pp

from_food_values <- values(grid_without_protection[["food"]], mat = FALSE)
to_food_values <- values(grid_with_protection[["food"]], mat = FALSE)
stable_background <- pmax(from_food_values, to_food_values, na.rm = TRUE) > .01

gradient_colour <- function(signal, value) {
  palette <- grDevices::colorRampPalette(c("#f7f7f7", transition_colours[[signal]]))(101)
  scaled <- pmin(value / transition_limits[[signal]], 1)
  palette[pmax(1L, pmin(101L, floor(sqrt(scaled) * 100) + 1L))]
}
dominant_fill <- rep(NA_character_, nrow(transition_values))
dominant_fill[stable_background & !material] <- "#F0F0F0"
for (signal in signal_columns) {
  selected <- material & dominant_signal == signal
  dominant_fill[selected] <- gradient_colour(signal, dominant_value[selected])
}
dominant_map_data <- transition_values %>%
  transmute(x, y, fill_colour = dominant_fill) %>%
  filter(!is.na(fill_colour))

wrapped_transition_labels <- vapply(
  transition_labels,
  function(z) paste(strwrap(z, width = 43), collapse = "\n"),
  character(1)
)
gradient_legend_data <- expand_grid(
  signal = factor(signal_columns, levels = rev(signal_columns)),
  intensity = seq(0, 1, length.out = 101)
) %>%
  mutate(
    signal_character = as.character(signal),
    fill_colour = mapply(
      function(s, z) gradient_colour(s, z * transition_limits[[s]]),
      signal_character, intensity, USE.NAMES = FALSE
    ),
    signal_label = factor(
      unname(wrapped_transition_labels[signal_character]),
      levels = unname(wrapped_transition_labels[rev(signal_columns)])
    )
  )
dominant_legend <- ggplot(
  gradient_legend_data, aes(intensity, signal_label, fill = fill_colour)
) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_continuous(
    breaks = c(0, 1), labels = c("Smaller", "Larger"), expand = c(0, 0)
  ) +
  labs(x = "Within-signal magnitude", y = NULL) +
  theme_minimal(base_size = 9.5) +
  theme(
    panel.grid = element_blank(), axis.title.x = element_text(face = "bold"),
    plot.margin = margin(2, 10, 4, 10)
  )

dominant_map <- ggplot(dominant_map_data, aes(x, y, fill = fill_colour)) +
  geom_tile(width = .5, height = .5) +
  geom_sf(
    data = world, fill = NA, colour = "grey20", linewidth = .12,
    inherit.aes = FALSE
  ) +
  scale_fill_identity() + coord_sf(expand = FALSE) +
  labs(
    title = "Dominant local cropland-transition signal after protection",
    subtitle = "Green = planted forest, orange = bioenergy, blue = expansion, grey = other decline; very pale grey = stable cropland"
  ) + map_theme
dominant_figure <- dominant_map / dominant_legend +
  plot_layout(heights = c(3.4, 1.25)) +
  plot_annotation(
    caption = paste0(
      "AIM-LU 0.5-degree 111-011 in ", target_year,
      ". Colour intensity is normalised within each signal to its 98th percentile. White cells have no material food-cropland transition signal."
    ),
    theme = theme(plot.caption = element_text(size = 8, colour = "grey35", hjust = 0))
  )
ggsave(
  file.path(
    transition_root, paste0("dominant_cropland_transition_signal_", target_year, ".png")
  ),
  dominant_figure, width = 13.5, height = 8.8, dpi = 300, bg = "white"
)

# A second map sacrifices magnitude detail for readability. A major transition
# changes at least 10 percentage points of the grid-cell area. A named forest
# or bioenergy conversion also requires that use itself to gain at least 10
# percentage points and account for at least 60% of the simultaneous
# forest-plus-bioenergy gain; otherwise it is labelled mixed.
discrete_min_change_pp <- 10
discrete_dominant_share <- .60
discrete_stable_pp <- 1

food_loss_values <- values(food_loss * 100, mat = FALSE)
food_gain_values <- values(food_gain * 100, mat = FALSE)
forest_gain_values <- values(forest_gain * 100, mat = FALSE)
bio_gain_values <- values(bio_gain * 100, mat = FALSE)
matched_loss_values <- values(matched_food_loss * 100, mat = FALSE)
unmatched_loss_values <- values(unmatched_food_loss * 100, mat = FALSE)
food_delta_values <- values(food_delta * 100, mat = FALSE)
land_values <- values(land_mask, mat = FALSE)
candidate_values <- forest_gain_values + bio_gain_values
forest_gain_share <- ifelse(candidate_values > 0, forest_gain_values / candidate_values, 0)
bio_gain_share <- ifelse(candidate_values > 0, bio_gain_values / candidate_values, 0)

transition_class <- rep(NA_character_, length(food_delta_values))
valid_land <- is.finite(land_values)
stable_cells <- valid_land & stable_background &
  abs(food_delta_values) <= discrete_stable_pp
transition_class[stable_cells] <- "Stable food cropland"

major_conversion <- valid_land &
  matched_loss_values >= discrete_min_change_pp
transition_class[major_conversion] <- "Cropland to mixed forest and bioenergy"
transition_class[major_conversion &
  forest_gain_values >= discrete_min_change_pp &
  forest_gain_share >= discrete_dominant_share] <- "Cropland to planted forest"
transition_class[major_conversion &
  bio_gain_values >= discrete_min_change_pp &
  bio_gain_share >= discrete_dominant_share] <- "Cropland to bioenergy"

major_other_decline <- valid_land &
  unmatched_loss_values >= discrete_min_change_pp &
  unmatched_loss_values >= matched_loss_values
transition_class[major_other_decline] <- "Other cropland decline"

major_expansion <- valid_land & food_gain_values >= discrete_min_change_pp
transition_class[major_expansion] <- "New or expanded food cropland"

discrete_levels <- c(
  "Cropland to planted forest",
  "Cropland to bioenergy",
  "Cropland to mixed forest and bioenergy",
  "New or expanded food cropland",
  "Other cropland decline",
  "Stable food cropland"
)
discrete_colours <- c(
  "Cropland to planted forest" = "#1B9E77",
  "Cropland to bioenergy" = "#E66101",
  "Cropland to mixed forest and bioenergy" = "#8E6BBE",
  "New or expanded food cropland" = "#2C7FB8",
  "Other cropland decline" = "#707070",
  "Stable food cropland" = "#F0F0F0"
)

discrete_map_data <- transition_values %>%
  transmute(
    x, y,
    transition = factor(transition_class, levels = discrete_levels)
  ) %>%
  filter(!is.na(transition))

discrete_transition_plot <- ggplot(
  discrete_map_data, aes(x, y, fill = transition)
) +
  geom_tile(width = .5, height = .5) +
  geom_sf(
    data = world, fill = NA, colour = "grey20", linewidth = .12,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = discrete_colours, drop = FALSE,
    name = "Dominant transition",
    labels = function(x) vapply(
      x, function(z) paste(strwrap(z, width = 31), collapse = "\n"),
      character(1)
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Major cropland transitions caused by adding protection",
    subtitle = paste0(
      "Discrete classes: at least ", discrete_min_change_pp,
      " percentage-point loss and named-use gain; dominant gain share at least ",
      scales::percent(discrete_dominant_share)
    ),
    caption = paste0(
      "AIM-LU 0.5-degree 111-011 in ", target_year,
      ". Pale grey is food cropland stable within +/-", discrete_stable_pp,
      " percentage point. Uncoloured cells have smaller or other changes. Same-cell co-change is a transition signal, not a traced land flow."
    )
  ) +
  map_theme +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8.5)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave(
  file.path(
    transition_root,
    paste0("major_cropland_transition_classes_", target_year, ".png")
  ),
  discrete_transition_plot, width = 13.5, height = 7.5,
  dpi = 300, bg = "white"
)

transition_class_raster <- food_delta
transition_codes <- match(transition_class, discrete_levels)
values(transition_class_raster) <- transition_codes
names(transition_class_raster) <- "major_cropland_transition_class"
writeRaster(
  transition_class_raster,
  file.path(data_root, paste0("major_cropland_transition_classes_", target_year, ".tif")),
  overwrite = TRUE
)

pixel_area_values <- values(pixel_area, mat = FALSE)
discrete_transition_summary <- tibble(
  transition = factor(transition_class, levels = discrete_levels),
  pixel_area_mha = pixel_area_values
) %>%
  filter(!is.na(transition), is.finite(pixel_area_mha)) %>%
  group_by(transition) %>%
  summarise(
    grid_cell_count = n(), cell_area_mha = sum(pixel_area_mha),
    .groups = "drop"
  ) %>%
  mutate(
    minimum_matched_change_pp = discrete_min_change_pp,
    dominant_gain_share = discrete_dominant_share,
    stable_band_pp = discrete_stable_pp
  )
write_csv(
  discrete_transition_summary,
  file.path(data_root, paste0("major_cropland_transition_class_summary_", target_year, ".csv"))
)

transition_summary <- lapply(signal_columns, function(signal) {
  signal_layer <- transition_signals[[signal]]
  tibble(
    signal, signal_label = unname(transition_labels[[signal]]),
    global_area_mha = global(signal_layer * pixel_area, "sum", na.rm = TRUE)[[1]],
    material_cell_area_mha = global(
      ifel(signal_layer * 100 > stable_threshold_pp, pixel_area, 0),
      "sum", na.rm = TRUE
    )[[1]],
    display_limit_pp = transition_limits[[signal]],
    material_threshold_pp = stable_threshold_pp
  )
}) %>% bind_rows()
write_csv(
  transition_summary,
  file.path(data_root, paste0("cropland_transition_signal_summary_", target_year, ".csv"))
)

# Apply the same source-loss accounting to bioenergy cropland and planted
# forest. These are same-cell co-change signals, not observed parcel flows.
land_identity_colours <- c(
  food = "#377EB8", bio = "#D95F02", planted_forest = "#1B9E77"
)

write_parallel_transition_outputs <- function(
    source_id, source_label, destination_ids, destination_labels,
    output_root, file_stub) {
  source_delta <- delta_layers[[source_id]]
  destination_delta <- delta_layers[[destination_ids]]
  source_loss <- ifel(source_delta < 0, -source_delta, 0)
  source_gain <- ifel(source_delta > 0, source_delta, 0)
  destination_gains <- ifel(destination_delta > 0, destination_delta, 0)
  candidate_gain <- app(destination_gains, sum, na.rm = TRUE)
  matched_loss <- ifel(
    candidate_gain > 0,
    ifel(source_loss < candidate_gain, source_loss, candidate_gain),
    0
  )

  allocated_loss <- destination_gains
  for (destination_id in destination_ids) {
    allocated_loss[[destination_id]] <- ifel(
      candidate_gain > 0,
      matched_loss * destination_gains[[destination_id]] / candidate_gain,
      0
    )
  }
  residual_loss <- source_loss - matched_loss
  expansion_id <- paste0(source_id, "_expansion")
  residual_id <- paste0("other_", source_id, "_decline")
  signal_ids <- c(
    paste0(source_id, "_to_", destination_ids), expansion_id, residual_id
  )
  signals <- c(allocated_loss, source_gain, residual_loss)
  names(signals) <- signal_ids
  signals <- mask(signals, land_mask)

  signal_labels <- c(
    setNames(
      paste0(
        source_label, " loss paired with ",
        tolower(unname(destination_labels[destination_ids])), " gain"
      ),
      paste0(source_id, "_to_", destination_ids)
    ),
    setNames(paste0("New or expanded ", tolower(source_label)), expansion_id),
    setNames(
      paste0(
        "Other ", tolower(source_label),
        " decline (unmatched to the two destination gains)"
      ),
      residual_id
    )
  )
  signal_colours <- c(
    setNames(unname(land_identity_colours[destination_ids]),
             paste0(source_id, "_to_", destination_ids)),
    setNames(unname(land_identity_colours[source_id]), expansion_id),
    setNames("#666666", residual_id)
  )

  writeRaster(
    signals,
    file.path(data_root, paste0(file_stub, "_transition_signals_", target_year, ".tif")),
    overwrite = TRUE
  )

  signal_limits <- vapply(signal_ids, function(signal) {
    v <- values(signals[[signal]] * 100, mat = FALSE)
    v <- v[is.finite(v) & v > 0]
    if (length(v) == 0L) 1 else unname(quantile(v, .98, na.rm = TRUE))
  }, numeric(1))

  stable_source <- mask(
    ifel(
      app(c(
        grid_without_protection[[source_id]],
        grid_with_protection[[source_id]]
      ), max, na.rm = TRUE) > .01 &
        app(signals, max, na.rm = TRUE) <= .001,
      1, NA
    ),
    land_mask
  )
  stable_source_data <- to_map_data(stable_source, "stable")

  signal_panels <- list()
  for (signal in signal_ids) {
    signal_data <- to_map_data(signals[[signal]] * 100, "signal_pp") %>%
      filter(signal_pp > 0)
    signal_panels[[signal]] <- ggplot() +
      geom_tile(
        data = stable_source_data, aes(x, y), width = .5, height = .5,
        fill = "grey94"
      ) +
      geom_tile(
        data = signal_data, aes(x, y, fill = signal_pp),
        width = .5, height = .5
      ) +
      geom_sf(
        data = world, fill = NA, colour = "grey20", linewidth = .12,
        inherit.aes = FALSE
      ) +
      scale_fill_gradient(
        low = "#f7f7f7", high = signal_colours[[signal]],
        limits = c(0, signal_limits[[signal]]), oob = squish,
        name = "Matched share\n(percentage points)"
      ) +
      coord_sf(expand = FALSE) +
      labs(
        title = unname(signal_labels[[signal]]),
        subtitle = paste0("Very pale grey marks stable ", tolower(source_label))
      ) + map_theme
  }

  panel_figure <- wrap_plots(signal_panels, ncol = 1) +
    plot_annotation(
      title = paste0(
        "Four spatial signals for ", tolower(source_label),
        " after adding protection"
      ),
      subtitle = "Protection is added to both CDR options: 111 minus 011",
      caption = paste0(
        "Same-cell co-change signals in ", target_year,
        ". Source loss is proportionally matched to simultaneous gains in the two other managed land uses. The grey residual can include natural-land retention but is not proof of it."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 10.5, colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
      )
    )
  ggsave(
    file.path(
      output_root, paste0(file_stub, "_transition_signals_four_panels_", target_year, ".png")
    ),
    panel_figure, width = 12, height = 16, dpi = 300, bg = "white"
  )

  signal_values <- as.data.frame(signals * 100, xy = TRUE, na.rm = FALSE)
  signal_matrix <- as.matrix(signal_values[, signal_ids])
  signal_matrix[!is.finite(signal_matrix)] <- 0
  dominant_index <- max.col(signal_matrix, ties.method = "first")
  dominant_value <- apply(signal_matrix, 1, max)
  dominant_signal <- signal_ids[dominant_index]
  material <- dominant_value > stable_threshold_pp
  from_values <- values(grid_without_protection[[source_id]], mat = FALSE)
  to_values <- values(grid_with_protection[[source_id]], mat = FALSE)
  stable_background <- pmax(from_values, to_values, na.rm = TRUE) > .01

  gradient_colour <- function(signal, value) {
    palette <- grDevices::colorRampPalette(
      c("#f7f7f7", signal_colours[[signal]])
    )(101)
    scaled <- pmin(value / signal_limits[[signal]], 1)
    palette[pmax(1L, pmin(101L, floor(sqrt(scaled) * 100) + 1L))]
  }
  dominant_fill <- rep(NA_character_, nrow(signal_values))
  dominant_fill[stable_background & !material] <- "#F0F0F0"
  for (signal in signal_ids) {
    selected <- material & dominant_signal == signal
    dominant_fill[selected] <- gradient_colour(
      signal, dominant_value[selected]
    )
  }
  dominant_map_data <- signal_values %>%
    transmute(x, y, fill_colour = dominant_fill) %>%
    filter(!is.na(fill_colour))

  wrapped_signal_labels <- vapply(
    signal_labels,
    function(z) paste(strwrap(z, width = 48), collapse = "\n"),
    character(1)
  )
  legend_data <- expand_grid(
    signal = factor(signal_ids, levels = rev(signal_ids)),
    intensity = seq(0, 1, length.out = 101)
  ) %>%
    mutate(
      signal_character = as.character(signal),
      fill_colour = mapply(
        function(s, z) gradient_colour(s, z * signal_limits[[s]]),
        signal_character, intensity, USE.NAMES = FALSE
      ),
      signal_label = factor(
        unname(wrapped_signal_labels[signal_character]),
        levels = unname(wrapped_signal_labels[rev(signal_ids)])
      )
    )
  dominant_legend <- ggplot(
    legend_data, aes(intensity, signal_label, fill = fill_colour)
  ) +
    geom_tile() + scale_fill_identity() +
    scale_x_continuous(
      breaks = c(0, 1), labels = c("Smaller", "Larger"),
      expand = c(0, 0)
    ) +
    labs(x = "Within-signal magnitude", y = NULL) +
    theme_minimal(base_size = 9.5) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(face = "bold"),
      plot.margin = margin(2, 10, 4, 10)
    )

  dominant_map <- ggplot(
    dominant_map_data, aes(x, y, fill = fill_colour)
  ) +
    geom_tile(width = .5, height = .5) +
    geom_sf(
      data = world, fill = NA, colour = "grey20", linewidth = .12,
      inherit.aes = FALSE
    ) +
    scale_fill_identity() + coord_sf(expand = FALSE) +
    labs(
      title = paste0(
        "Dominant local ", tolower(source_label),
        " transition signal after protection"
      ),
      subtitle = "Hue identifies the destination or expansion; grey is other decline; very pale grey is stable"
    ) + map_theme
  dominant_figure <- dominant_map / dominant_legend +
    plot_layout(heights = c(3.4, 1.25)) +
    plot_annotation(
      caption = paste0(
        "AIM-LU 0.5-degree 111-011 in ", target_year,
        ". Colour intensity is normalised within each signal to its 98th percentile."
      ),
      theme = theme(
        plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
      )
    )
  ggsave(
    file.path(
      output_root, paste0("dominant_", file_stub, "_transition_signal_", target_year, ".png")
    ),
    dominant_figure, width = 13.5, height = 8.8,
    dpi = 300, bg = "white"
  )

  source_loss_values <- values(source_loss * 100, mat = FALSE)
  source_gain_values <- values(source_gain * 100, mat = FALSE)
  matched_loss_values <- values(matched_loss * 100, mat = FALSE)
  residual_loss_values <- values(residual_loss * 100, mat = FALSE)
  destination_gain_values <- as.data.frame(
    values(destination_gains * 100, mat = TRUE)
  )
  names(destination_gain_values) <- destination_ids
  candidate_values <- rowSums(destination_gain_values, na.rm = TRUE)
  land_values <- values(land_mask, mat = FALSE)
  valid_land <- is.finite(land_values)

  short_land_labels <- c(
    food = "Food cropland", bio = "Bioenergy",
    planted_forest = "Planted forest"
  )
  class_labels <- c(
    setNames(
      paste0(
        short_land_labels[[source_id]], " to ",
        tolower(unname(short_land_labels[destination_ids]))
      ),
      destination_ids
    )
  )
  mixed_label <- paste0(short_land_labels[[source_id]], " to mixed uses")
  expansion_label <- paste0(short_land_labels[[source_id]], " expansion")
  residual_label <- paste0("Other ", tolower(short_land_labels[[source_id]]), " decline")
  stable_label <- paste0("Stable ", tolower(short_land_labels[[source_id]]))
  discrete_levels <- c(
    unname(class_labels), mixed_label, expansion_label,
    residual_label, stable_label
  )
  discrete_colours <- c(
    setNames(unname(land_identity_colours[destination_ids]), unname(class_labels)),
    setNames("#8E6BBE", mixed_label),
    setNames(unname(land_identity_colours[source_id]), expansion_label),
    setNames("#707070", residual_label),
    setNames("#F0F0F0", stable_label)
  )

  transition_class <- rep(NA_character_, length(source_loss_values))
  stable_cells <- valid_land & stable_background &
    abs(values(source_delta * 100, mat = FALSE)) <= discrete_stable_pp
  transition_class[stable_cells] <- stable_label
  major_conversion <- valid_land &
    matched_loss_values >= discrete_min_change_pp
  transition_class[major_conversion] <- mixed_label
  for (destination_id in destination_ids) {
    gain_values <- destination_gain_values[[destination_id]]
    gain_share <- ifelse(candidate_values > 0,
                         gain_values / candidate_values, 0)
    named_conversion <- major_conversion &
      gain_values >= discrete_min_change_pp &
      gain_share >= discrete_dominant_share
    transition_class[named_conversion] <- class_labels[[destination_id]]
  }
  major_residual <- valid_land &
    residual_loss_values >= discrete_min_change_pp &
    residual_loss_values >= matched_loss_values
  transition_class[major_residual] <- residual_label
  major_expansion <- valid_land &
    source_gain_values >= discrete_min_change_pp
  transition_class[major_expansion] <- expansion_label

  discrete_map_data <- signal_values %>%
    transmute(
      x, y,
      transition = factor(transition_class, levels = discrete_levels)
    ) %>%
    filter(!is.na(transition))
  discrete_plot <- ggplot(
    discrete_map_data, aes(x, y, fill = transition)
  ) +
    geom_tile(width = .5, height = .5) +
    geom_sf(
      data = world, fill = NA, colour = "grey20", linewidth = .12,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = discrete_colours, drop = FALSE,
      name = "Dominant transition",
      labels = function(x) vapply(
        x, function(z) paste(strwrap(z, width = 28), collapse = "\n"),
        character(1)
      )
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = paste0("Major transitions of ", tolower(source_label),
                     " after adding protection"),
      subtitle = paste0(
        "Discrete classes: at least ", discrete_min_change_pp,
        " percentage-point loss and named-use gain; dominant gain share at least ",
        scales::percent(discrete_dominant_share)
      ),
      caption = paste0(
        "AIM-LU 0.5-degree 111-011 in ", target_year,
        ". Other decline is unmatched to gains in the two managed destinations and may include natural-land retention; it is not a direct protected-area overlap."
      )
    ) + map_theme +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8.5)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  ggsave(
    file.path(
      output_root, paste0("major_", file_stub, "_transition_classes_", target_year, ".png")
    ),
    discrete_plot, width = 13.5, height = 7.5,
    dpi = 300, bg = "white"
  )

  class_raster <- source_delta
  values(class_raster) <- match(transition_class, discrete_levels)
  names(class_raster) <- paste0("major_", file_stub, "_transition_class")
  writeRaster(
    class_raster,
    file.path(
      data_root, paste0("major_", file_stub, "_transition_classes_", target_year, ".tif")
    ),
    overwrite = TRUE
  )

  pixel_area_values <- values(pixel_area, mat = FALSE)
  class_summary <- tibble(
    transition = factor(transition_class, levels = discrete_levels),
    pixel_area_mha = pixel_area_values
  ) %>%
    filter(!is.na(transition), is.finite(pixel_area_mha)) %>%
    group_by(transition) %>%
    summarise(
      grid_cell_count = n(), cell_area_mha = sum(pixel_area_mha),
      .groups = "drop"
    ) %>%
    mutate(
      minimum_matched_change_pp = discrete_min_change_pp,
      dominant_gain_share = discrete_dominant_share,
      stable_band_pp = discrete_stable_pp
    )
  write_csv(
    class_summary,
    file.path(
      data_root, paste0("major_", file_stub, "_transition_class_summary_", target_year, ".csv")
    )
  )

  signal_summary <- lapply(signal_ids, function(signal) {
    signal_layer <- signals[[signal]]
    tibble(
      source_land_use = source_id,
      signal, signal_label = unname(signal_labels[[signal]]),
      global_area_mha = global(
        signal_layer * pixel_area, "sum", na.rm = TRUE
      )[[1]],
      material_cell_area_mha = global(
        ifel(signal_layer * 100 > stable_threshold_pp, pixel_area, 0),
        "sum", na.rm = TRUE
      )[[1]],
      display_limit_pp = signal_limits[[signal]],
      material_threshold_pp = stable_threshold_pp
    )
  }) %>% bind_rows()
  write_csv(
    signal_summary,
    file.path(
      data_root, paste0(file_stub, "_transition_signal_summary_", target_year, ".csv")
    )
  )
}

write_parallel_transition_outputs(
  source_id = "bio",
  source_label = "Bioenergy cropland",
  destination_ids = c("food", "planted_forest"),
  destination_labels = c(
    food = "Food cropland", planted_forest = "Planted forest"
  ),
  output_root = bio_transition_root,
  file_stub = "bioenergy_cropland"
)

write_parallel_transition_outputs(
  source_id = "planted_forest",
  source_label = "Planted forest",
  destination_ids = c("food", "bio"),
  destination_labels = c(
    food = "Food cropland", bio = "Bioenergy cropland"
  ),
  output_root = forest_transition_root,
  file_stub = "planted_forest"
)

# 5. Forest and grassland loss avoided by protection ----------------------

# BtC2 class definitions from luwwfnum_btc2.map:
# 3 = managed grassland/pasture, 4 = naturally regenerating forest,
# 9 = natural grassland plus other natural land (GL + OL).
vegetation_classes <- c(
  natural_forest = 4L,
  managed_grassland = 3L,
  natural_grassland_other = 9L
)
vegetation_labels <- c(
  natural_forest = "Naturally regenerating forest",
  managed_grassland = "Managed grassland / pasture",
  natural_grassland_other = "Natural grassland + other natural land"
)

read_grid_year_classes <- function(state, year, classes) {
  path <- file.path(
    data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[[state]], ".nc")
  )
  x <- rast(path, subds = "LC_area_share")
  position <- match(year, grid_years)
  layers <- x[[(unname(classes) - 1L) * length(grid_years) + position]] /
    grid_export_duplicate_factor
  names(layers) <- names(classes)
  layers
}

vegetation_base_year <- 2005L
vegetation_base_without <- read_grid_year_classes(
  "011", vegetation_base_year, vegetation_classes
)
vegetation_base_with <- read_grid_year_classes(
  "111", vegetation_base_year, vegetation_classes
)
vegetation_target_without <- read_grid_year_classes(
  "011", target_year, vegetation_classes
)
vegetation_target_with <- read_grid_year_classes(
  "111", target_year, vegetation_classes
)

vegetation_loss_without_delta <- vegetation_base_without - vegetation_target_without
vegetation_loss_with_delta <- vegetation_base_without - vegetation_target_with
loss_without <- ifel(vegetation_loss_without_delta > 0, vegetation_loss_without_delta, 0)
loss_with <- ifel(vegetation_loss_with_delta > 0, vegetation_loss_with_delta, 0)
loss_effect <- loss_without - loss_with
avoided_loss <- ifel(loss_effect > 0, loss_effect, 0)
additional_loss <- ifel(loss_effect < 0, -loss_effect, 0)
names(loss_without) <- names(vegetation_classes)
names(loss_with) <- names(vegetation_classes)
names(avoided_loss) <- names(vegetation_classes)
names(additional_loss) <- names(vegetation_classes)
names(loss_effect) <- names(vegetation_classes)
writeRaster(
  c(loss_without, loss_with, avoided_loss, additional_loss, loss_effect),
  file.path(data_root, paste0("vegetation_loss_effects_", target_year, ".tif")),
  overwrite = TRUE
)

vegetation_rows <- list()
avoided_maps <- list()
effect_maps <- list()
loss_rows <- list()
for (class_id in names(vegetation_classes)) {
  without_layer <- mask(loss_without[[class_id]] * 100, land_mask)
  with_layer <- mask(loss_with[[class_id]] * 100, land_mask)
  avoided_layer <- mask(avoided_loss[[class_id]] * 100, land_mask)
  effect_layer <- mask(loss_effect[[class_id]] * 100, land_mask)
  all_loss_values <- c(
    values(without_layer, mat = FALSE), values(with_layer, mat = FALSE)
  )
  all_loss_values <- all_loss_values[is.finite(all_loss_values) & all_loss_values > 0]
  limit <- if (length(all_loss_values) == 0L) 1 else
    unname(quantile(all_loss_values, .98, na.rm = TRUE))
  effect_values <- values(effect_layer, mat = FALSE)
  effect_values <- effect_values[is.finite(effect_values) & effect_values != 0]
  effect_limit <- if (length(effect_values) == 0L) 1 else
    unname(quantile(abs(effect_values), .98, na.rm = TRUE))

  without_data <- to_map_data(without_layer, "value") %>% filter(value > 0)
  with_data <- to_map_data(with_layer, "value") %>% filter(value > 0)
  avoided_data <- to_map_data(avoided_layer, "value") %>% filter(value > 0)
  effect_data <- to_map_data(effect_layer, "value") %>% filter(value != 0)
  label <- unname(vegetation_labels[[class_id]])

  p_without <- plot_continuous_map(
    without_data, "value", "Loss without protection", paste0(vegetation_base_year, " to ", target_year, ", scenario 011"),
    c("#fff5f0", "#cb181d"), c(0, limit), "Loss\n(percentage points)"
  ) + theme(legend.position = "none")
  p_with <- plot_continuous_map(
    with_data, "value", "Loss with protection", paste0(vegetation_base_year, " to ", target_year, ", scenario 111"),
    c("#fff5f0", "#cb181d"), c(0, limit), "Loss\n(percentage points)"
  )
  p_effect <- plot_continuous_map(
    effect_data, "value", "Protection effect on loss",
    "Green = avoided loss; red = additional loss elsewhere",
    c("#b2182b", "#f7f7f7", "#1a9850"),
    c(-effect_limit, effect_limit), "Loss effect\n(percentage points)"
  )
  p_avoided <- plot_continuous_map(
    avoided_data, "value", "Loss avoided by protection",
    paste0("Vegetation retained relative to the common ", vegetation_base_year, " baseline"),
    c("#f7fcf5", "#006d2c"), c(0, effect_limit),
    "Avoided loss\n(percentage points)"
  )
  vegetation_rows[[class_id]] <- p_without + p_with + p_effect +
    plot_layout(widths = c(1, 1.05, 1.05)) +
    plot_annotation(
      title = label,
      theme = theme(plot.title = element_text(face = "bold", size = 13))
    )
  avoided_maps[[class_id]] <- p_avoided +
    labs(
      title = label,
      subtitle = paste0(vegetation_base_year, " vegetation loss avoided in 111 relative to 011")
    )
  effect_maps[[class_id]] <- p_effect + labs(title = label)

  loss_rows[[class_id]] <- tibble(
    class_id, vegetation_class = label,
    loss_without_protection_mha = global(
      loss_without[[class_id]] * pixel_area, "sum", na.rm = TRUE
    )[[1]],
    loss_with_protection_mha = global(
      loss_with[[class_id]] * pixel_area, "sum", na.rm = TRUE
    )[[1]],
    avoided_loss_mha = global(
      avoided_loss[[class_id]] * pixel_area, "sum", na.rm = TRUE
    )[[1]],
    additional_loss_elsewhere_mha = global(
      additional_loss[[class_id]] * pixel_area, "sum", na.rm = TRUE
    )[[1]],
    net_loss_avoided_mha = global(
      loss_effect[[class_id]] * pixel_area, "sum", na.rm = TRUE
    )[[1]],
    direct_target_difference_mha = global(
      (vegetation_target_with[[class_id]] - vegetation_target_without[[class_id]]) *
        pixel_area,
      "sum", na.rm = TRUE
    )[[1]],
    max_abs_base_scenario_difference_pp = global(
      abs(vegetation_base_with[[class_id]] - vegetation_base_without[[class_id]]) * 100,
      "max", na.rm = TRUE
    )[[1]],
    loss_display_limit_pp = limit,
    effect_display_limit_pp = effect_limit
  )
}

vegetation_triptych <- wrap_plots(vegetation_rows, ncol = 1) +
  plot_annotation(
    title = paste0("How does protection change forest and grassland loss since ", vegetation_base_year, "?"),
    subtitle = "The third column separates locally avoided loss (green) from additional loss elsewhere (red)",
    caption = paste0(
      "AIM-LU 0.5-degree land shares. Class 9 combines natural grassland (GL) and other natural land (OL), so grassland cannot be isolated from this NetCDF."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10.5, colour = "grey30"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
    )
  )
ggsave(
  file.path(
    vegetation_root, paste0("forest_grassland_loss_without_with_effect_", target_year, ".png")
  ),
  vegetation_triptych, width = 16.5, height = 15.5, dpi = 300, bg = "white"
)

avoided_only_figure <- wrap_plots(avoided_maps, ncol = 1) +
  plot_annotation(
    title = "Vegetation retained by adding protection under mitigation",
    subtitle = paste0(vegetation_base_year, "-to-", target_year, " loss avoided in 111 relative to 011"),
    caption = "Green shows where baseline vegetation would decline more without Protection. It is an avoided-loss counterfactual, not a direct protected-area intersection.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10.5, colour = "grey30"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
    )
  )
ggsave(
  file.path(
    vegetation_root, paste0("vegetation_retained_by_protection_", target_year, ".png")
  ),
  avoided_only_figure, width = 11.5, height = 12.8, dpi = 300, bg = "white"
)

retained_displaced_figure <- wrap_plots(effect_maps, ncol = 1) +
  plot_annotation(
    title = "Protection retains vegetation locally but can displace loss elsewhere",
    subtitle = paste0(
      "Change in ", vegetation_base_year, "-to-", target_year,
      " loss: green = less loss in 111; red = more loss in 111"
    ),
    caption = "This counterfactual map identifies spatial redistribution of vegetation loss; it does not trace a physical flow between individual cells.",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 10.5, colour = "grey30"),
      plot.caption = element_text(size = 8, colour = "grey35", hjust = 0)
    )
  )
ggsave(
  file.path(
    vegetation_root,
    paste0("vegetation_retained_and_displaced_by_protection_", target_year, ".png")
  ),
  retained_displaced_figure, width = 11.5, height = 12.8,
  dpi = 300, bg = "white"
)

vegetation_summary <- bind_rows(loss_rows) %>%
  mutate(
    loss_reduction_pct = if_else(
      loss_without_protection_mha > 1e-10,
      100 * net_loss_avoided_mha / loss_without_protection_mha, NA_real_
    ),
    gross_avoided_loss_pct = if_else(
      loss_without_protection_mha > 1e-10,
      100 * avoided_loss_mha / loss_without_protection_mha, NA_real_
    )
  )
write_csv(
  vegetation_summary,
  file.path(data_root, paste0("vegetation_loss_effect_summary_", target_year, ".csv"))
)

message("Wrote bivariate spatial-reallocation outputs to ", analysis_root)
