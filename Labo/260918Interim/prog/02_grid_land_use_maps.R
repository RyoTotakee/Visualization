suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
})

source("prog/common.R")

grid_output_dir <- file.path(output_dir, "02_grid_land_use")
dir.create(grid_output_dir, recursive = TRUE, showWarnings = FALSE)

grid_scenarios <- scenario_states("global2")
grid_years <- c(2005, 2010, seq(2020, 2100, 10))
year_position <- match(target_year, grid_years)
land_classes <- c(
  `Food cropland` = 1L,
  `2G bioenergy` = 2L,
  `Planted forest` = 5L
)

# TEMPORARY WORKAROUND: existing AIM-LUmap files double-count Rbasin and R17.
# Remove this factor after regenerating them with the corrected combine.gms.
grid_export_duplicate_factor <- 2

read_land_classes <- function(scenario, classes = land_classes) {
  nc_path <- file.path(data_dir, "PLUM", "nc", paste0("AIM-LUmap_", scenario, ".nc"))
  all_layers <- rast(nc_path, subds = "LC_area_share")
  layer_positions <- (unname(classes) - 1L) * length(grid_years) + year_position
  selected <- all_layers[[layer_positions]] / grid_export_duplicate_factor
  names(selected) <- names(classes)
  selected
}

land_rasters <- lapply(grid_scenarios, read_land_classes)

# The mask uses all land-cover classes in BaU so that oceans are transparent.
baseline_file <- file.path(
  data_dir, "PLUM", "nc", paste0("AIM-LUmap_", grid_scenarios[["000"]], ".nc")
)
baseline_all <- rast(baseline_file, subds = "LC_area_share")
mask_positions <- (0:11) * length(grid_years) + year_position
land_mask <- app(baseline_all[[mask_positions]], sum, na.rm = TRUE)
land_mask[land_mask <= 0] <- NA

grid_effects <- list(
  `Protection under mitigation` = land_rasters[["111"]] - land_rasters[["011"]],
  `Afforestation contribution` = land_rasters[["011"]] - land_rasters[["001"]],
  `BECCS contribution` = land_rasters[["011"]] - land_rasters[["010"]],
  `Protection interaction` =
    (land_rasters[["111"]] - land_rasters[["011"]]) -
    (land_rasters[["100"]] - land_rasters[["000"]]),
  `Combined policy` = land_rasters[["111"]] - land_rasters[["000"]]
)

world <- ne_countries(scale = "small", returnclass = "sf")
pixel_area <- rast(baseline_file, subds = "pixel_area")
area_summary <- list()

for (contrast_name in names(grid_effects)) {
  change <- mask(grid_effects[[contrast_name]] * 100, land_mask)
  names(change) <- names(land_classes)

  plot_data <- as.data.frame(change, xy = TRUE, na.rm = TRUE, wide = FALSE) %>%
    rename(land_class = layer, change_pp = values)
  plot_data$land_class <- factor(plot_data$land_class, levels = names(land_classes))
  fill_limit <- symmetric_limit(plot_data$change_pp)

  map_plot <- ggplot(plot_data, aes(x = x, y = y, fill = change_pp)) +
    geom_tile(width = 0.5, height = 0.5) +
    geom_sf(data = world, fill = NA, colour = "grey25", linewidth = 0.15,
            inherit.aes = FALSE) +
    facet_wrap(vars(land_class), ncol = 1) +
    scale_fill_gradient2(
      low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
      midpoint = 0, limits = c(-fill_limit, fill_limit), oob = squish,
      name = "Percentage\npoints"
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1.5, "mm")
    )

  file_stub <- gsub("[^a-z0-9]+", "_", tolower(contrast_name))
  ggsave(
    file.path(grid_output_dir, paste0(file_stub, "_", target_year, ".png")),
    map_plot, width = 10.5, height = 10.8, dpi = 300, bg = "white"
  )

  area_change <- global((change / 100) * pixel_area, "sum", na.rm = TRUE) %>%
    tibble::rownames_to_column("land_class") %>%
    rename(change_million_ha = sum) %>%
    mutate(contrast = contrast_name, .before = 1)
  area_summary[[contrast_name]] <- area_change
}

bind_rows(area_summary) %>%
  write_csv(file.path(grid_output_dir, paste0("global_area_changes_", target_year, ".csv")))
