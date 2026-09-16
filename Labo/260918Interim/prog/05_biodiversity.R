suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
  library(tidyr)
})

source("prog/common.R")

bii_output_dir <- file.path(output_dir, "05_biodiversity")
dir.create(bii_output_dir, recursive = TRUE, showWarnings = FALSE)

bii_scenarios <- scenario_states("global2")
bii_years <- c(2005, 2010, seq(2020, 2100, 10))
bii_year_position <- match(target_year, bii_years)

read_bii <- function(scenario) {
  nc_path <- file.path(
    data_dir, "BII", "nc", paste0("BIImap_", scenario, "_default.nc")
  )
  bii <- rast(nc_path, subds = "BII")[[bii_year_position]]
  names(bii) <- scenario
  bii
}

bii_rasters <- lapply(bii_scenarios, read_bii)

# Exact three-policy Shapley decomposition at every grid cell.
bii_protection <-
  (bii_rasters[["100"]] - bii_rasters[["000"]]) / 3 +
  ((bii_rasters[["110"]] - bii_rasters[["010"]]) +
     (bii_rasters[["101"]] - bii_rasters[["001"]])) / 6 +
  (bii_rasters[["111"]] - bii_rasters[["011"]]) / 3
bii_afforestation <-
  (bii_rasters[["010"]] - bii_rasters[["000"]]) / 3 +
  ((bii_rasters[["110"]] - bii_rasters[["100"]]) +
     (bii_rasters[["011"]] - bii_rasters[["001"]])) / 6 +
  (bii_rasters[["111"]] - bii_rasters[["101"]]) / 3
bii_beccs <-
  (bii_rasters[["001"]] - bii_rasters[["000"]]) / 3 +
  ((bii_rasters[["101"]] - bii_rasters[["100"]]) +
     (bii_rasters[["011"]] - bii_rasters[["010"]])) / 6 +
  (bii_rasters[["111"]] - bii_rasters[["110"]]) / 3
bii_total <- bii_rasters[["111"]] - bii_rasters[["000"]]

bii_effects <- c(
  bii_protection, bii_afforestation, bii_beccs, bii_total
) * 100
names(bii_effects) <- c("Protected areas", "Afforestation", "BECCS", "Combined policy")

bii_plot_data <- as.data.frame(
  bii_effects, xy = TRUE, na.rm = TRUE, wide = FALSE
) %>%
  rename(policy = layer, change_pp = values) %>%
  mutate(policy = factor(policy, levels = names(bii_effects)))

bii_limit <- symmetric_limit(bii_plot_data$change_pp)
world <- ne_countries(scale = "small", returnclass = "sf")

bii_map <- ggplot(bii_plot_data, aes(x, y, fill = change_pp)) +
  geom_tile(width = 0.5, height = 0.5) +
  geom_sf(data = world, fill = NA, colour = "grey25", linewidth = 0.15,
          inherit.aes = FALSE) +
  facet_wrap(vars(policy), ncol = 2) +
  scale_fill_gradient2(
    low = "#8c2d04", mid = "#f7f7f7", high = "#238b45",
    midpoint = 0, limits = c(-bii_limit, bii_limit), oob = squish,
    name = "BII points"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.5, "mm")
  )

ggsave(
  file.path(bii_output_dir, paste0("bii_grid_shapley_", target_year, ".png")),
  bii_map, width = 12, height = 6.8, dpi = 300, bg = "white"
)

# Area-weighted global mean contributions.
cell_area <- cellSize(bii_effects[[1]], unit = "km")
global_bii <- lapply(seq_len(nlyr(bii_effects)), function(i) {
  valid_area <- ifel(is.na(bii_effects[[i]]), NA, cell_area)
  weighted_sum <- global(bii_effects[[i]] * valid_area, "sum", na.rm = TRUE)[[1]]
  area_sum <- global(valid_area, "sum", na.rm = TRUE)[[1]]
  tibble(policy = names(bii_effects)[[i]], change_bii_points = weighted_sum / area_sum)
}) %>% bind_rows()
write_csv(
  global_bii,
  file.path(bii_output_dir, paste0("bii_global_shapley_", target_year, ".csv"))
)

# Region-level area-weighted BII ---------------------------------------------

bii_csv_files <- list.files(
  file.path(data_dir, "BII", "csv"),
  pattern = "^BII_regionagg_.*_default_area\\.csv$",
  full.names = TRUE
)

bii_region <- lapply(bii_csv_files, function(csv_file) {
  read_csv(csv_file, show_col_types = FALSE) %>%
    transmute(
      region,
      scenario = sub("_default$", "", scenario),
      value = .data[[paste0("v_", target_year)]]
    )
  }) %>%
  bind_rows() %>%
  filter(scenario %in% bii_scenarios) %>%
  mutate(state = state_from_scenario(scenario, "global2")) %>%
  select(region, state, value) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  filter(if_all(all_of(names(bii_scenarios)), is.finite)) %>%
  shapley_three() %>%
  transmute(
    region,
    protection = 100 * protection,
    afforestation = 100 * afforestation,
    beccs = 100 * beccs,
    total = 100 * total,
    baseline,
    final
  )

write_csv(
  bii_region,
  file.path(bii_output_dir, paste0("bii_region_shapley_", target_year, ".csv"))
)

policy_labels <- c(
  protection = "Protected areas",
  afforestation = "Afforestation",
  beccs = "BECCS"
)
bii_top <- bii_region %>%
  slice_max(abs(total), n = 20, with_ties = FALSE) %>%
  arrange(total) %>%
  mutate(region = factor(region, levels = region))

bii_bars <- bii_top %>%
  select(region, all_of(names(policy_labels))) %>%
  pivot_longer(
    all_of(names(policy_labels)),
    names_to = "policy", values_to = "contribution"
  ) %>%
  mutate(policy = factor(policy_labels[policy], levels = policy_labels)) %>%
  ggplot(aes(region, contribution, fill = policy)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.72) +
  geom_point(
    data = bii_top, aes(region, total), inherit.aes = FALSE,
    shape = 18, size = 2.2, colour = "black"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#756bb1", "#31a354", "#e6550d"), name = NULL) +
  labs(x = NULL, y = "Contribution (BII points)") +
  theme_paper() +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(bii_output_dir, paste0("bii_region_shapley_bars_", target_year, ".png")),
  bii_bars, width = 8.5, height = 7, dpi = 300, bg = "white"
)

unlink(file.path(
  bii_output_dir,
  c(
    paste0("bii_country_shapley_", target_year, ".csv"),
    paste0("bii_country_shapley_bars_", target_year, ".png")
  )
))
