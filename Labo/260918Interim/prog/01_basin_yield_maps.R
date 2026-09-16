suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(sf)
  library(tidyr)
})

source("prog/common.R")
sf_use_s2(FALSE)
init_gdx()

basin_root_dir <- file.path(output_dir, "01_basin_yield")
basin_output_dir <- file.path(basin_root_dir, "aggregate_food_crops")
dir.create(basin_output_dir, recursive = TRUE, showWarnings = FALSE)

# Remove flat files produced before aggregate and commodity outputs were split.
flat_outputs <- list.files(basin_root_dir, full.names = TRUE, recursive = FALSE)
unlink(flat_outputs[file.exists(flat_outputs) & !dir.exists(flat_outputs)])

basins <- st_read(
  file.path(data_dir, "Basin_Boundary", "Basin_Boundary.shp"),
  quiet = TRUE
)

# ALPHA yldBas_load -----------------------------------------------------------

alpha_scenarios <- scenario_states("global2")

alpha_yield <- rgdx.param(
  file.path(data_dir, "ALPHA", "all_scenario_World.gdx"),
  "yldBas_load"
) %>%
  filter(
    SCENARIO %in% alpha_scenarios,
    as.integer(as.character(Y)) == target_year,
    c %in% food_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO, "global2"),
    crop = c,
    Basin = bas,
    ISO_A3 = cty,
    yield = yldBas_load
  )

# A fixed BaU harvested-area basket prevents crop-composition changes from
# being mistaken for changes in physical yield.
alpha_weights <- rgdx.param(
  file.path(data_dir, "ALPHA", "all_scenario_World.gdx"),
  "qlBas_load"
) %>%
  filter(
    SCENARIO == alpha_scenarios[["000"]],
    as.integer(as.character(Y)) == target_year,
    c %in% food_crop_codes
  ) %>%
  transmute(crop = c, Basin = bas, ISO_A3 = cty, weight = qlBas_load)

alpha_basin <- alpha_yield %>%
  left_join(alpha_weights, by = c("crop", "Basin", "ISO_A3")) %>%
  filter(is.finite(weight), weight > 0) %>%
  group_by(state, Basin, ISO_A3) %>%
  summarise(value = weighted.mean(yield, weight), .groups = "drop") %>%
  complete(state = names(alpha_scenarios), nesting(Basin, ISO_A3)) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  transmute(
    Basin,
    ISO_A3,
    `Protection without mitigation` = 100 * (`100` / `000` - 1),
    `Protection under mitigation` = 100 * (`111` / `011` - 1),
    `Protection interaction` =
      100 * ((`111` - `011`) - (`100` - `000`)) / `000`,
    `Afforestation contribution` = 100 * (`011` / `001` - 1),
    `BECCS contribution` = 100 * (`011` / `010` - 1),
    `Combined policy` = 100 * (`111` / `000` - 1)
  ) %>%
  pivot_longer(-c(Basin, ISO_A3), names_to = "contrast", values_to = "change_pct") %>%
  mutate(contrast = factor(contrast, levels = c(
    "Protection without mitigation", "Protection under mitigation",
    "Protection interaction", "Afforestation contribution",
    "BECCS contribution", "Combined policy"
  )))

alpha_map <- basins %>%
  left_join(alpha_basin, by = c("Basin", "ISO_A3"))

alpha_limit <- symmetric_limit(alpha_map$change_pct)
alpha_plot <- ggplot() +
  geom_sf(data = basins, fill = "grey92", colour = NA) +
  geom_sf(
    data = alpha_map %>% filter(!is.na(contrast)),
    aes(fill = change_pct), colour = NA
  ) +
  facet_wrap(vars(contrast), ncol = 3) +
  scale_fill_gradient2(
    low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
    midpoint = 0, limits = c(-alpha_limit, alpha_limit),
    oob = squish, name = "%"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2, "mm")
  )

ggsave(
  file.path(basin_output_dir, paste0("alpha_yldBas_effects_", target_year, ".png")),
  alpha_plot, width = 13.5, height = 6.8, dpi = 300, bg = "white"
)
write_csv(st_drop_geometry(alpha_map) %>% filter(!is.na(contrast)), file.path(
  basin_output_dir, paste0("alpha_yldBas_effects_", target_year, ".csv")
))

rm(alpha_yield, alpha_weights, alpha_basin, alpha_map)
gc()

# PLUM yldPlum ---------------------------------------------------------------

# plum_data.gdx contains LandInt_global1 by design, as specified in README.md.
plum_scenarios <- scenario_states("global1")

plum_yield <- rgdx.param(
  file.path(data_dir, "ALPHA", "plum_data.gdx"),
  "yldPlum"
) %>%
  filter(
    sce_plum %in% plum_scenarios,
    as.integer(as.character(yr)) == target_year,
    a_cro %in% food_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(sce_plum, "global1"),
    crop = a_cro,
    Basin = bas,
    ISO_A3 = cty,
    yield_index = yldPlum
  )

plum_weights <- rgdx.param(
  file.path(data_dir, "ALPHA", "all_scenario_World.gdx"),
  "qlBas_load"
) %>%
  filter(
    SCENARIO == plum_scenarios[["000"]],
    as.integer(as.character(Y)) == target_year,
    c %in% food_crop_codes
  ) %>%
  transmute(crop = c, Basin = bas, ISO_A3 = cty, weight = qlBas_load)

plum_basin <- plum_yield %>%
  left_join(plum_weights, by = c("crop", "Basin", "ISO_A3")) %>%
  filter(is.finite(weight), weight > 0) %>%
  group_by(state, Basin, ISO_A3) %>%
  summarise(value = weighted.mean(yield_index, weight), .groups = "drop") %>%
  complete(state = names(plum_scenarios), nesting(Basin, ISO_A3)) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  transmute(
    Basin,
    ISO_A3,
    `Protection without mitigation` = 100 * (`100` / `000` - 1),
    `Protection under mitigation` = 100 * (`111` / `011` - 1),
    `Protection interaction` =
      100 * ((`111` - `011`) - (`100` - `000`)) / `000`,
    `Afforestation contribution` = 100 * (`011` / `001` - 1),
    `BECCS contribution` = 100 * (`011` / `010` - 1),
    `Combined policy` = 100 * (`111` / `000` - 1)
  ) %>%
  pivot_longer(-c(Basin, ISO_A3), names_to = "contrast", values_to = "change_pct") %>%
  mutate(contrast = factor(contrast, levels = c(
    "Protection without mitigation", "Protection under mitigation",
    "Protection interaction", "Afforestation contribution",
    "BECCS contribution", "Combined policy"
  )))

plum_map <- basins %>%
  left_join(plum_basin, by = c("Basin", "ISO_A3"))

plum_limit <- symmetric_limit(plum_map$change_pct)
plum_plot <- ggplot() +
  geom_sf(data = basins, fill = "grey92", colour = NA) +
  geom_sf(
    data = plum_map %>% filter(!is.na(contrast)),
    aes(fill = change_pct), colour = NA
  ) +
  facet_wrap(vars(contrast), ncol = 3) +
  scale_fill_gradient2(
    low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
    midpoint = 0, limits = c(-plum_limit, plum_limit),
    oob = squish, name = "%"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2, "mm")
  )

ggsave(
  file.path(basin_output_dir, paste0("plum_yldPlum_effects_", target_year, ".png")),
  plum_plot, width = 13.5, height = 6.8, dpi = 300, bg = "white"
)
write_csv(st_drop_geometry(plum_map) %>% filter(!is.na(contrast)), file.path(
  basin_output_dir, paste0("plum_yldPlum_effects_", target_year, ".csv")
))
