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

commodity_root <- file.path(output_dir, "01_basin_yield", "by_commodity")
dir.create(commodity_root, recursive = TRUE, showWarnings = FALSE)

basins <- st_read(
  file.path(data_dir, "Basin_Boundary", "Basin_Boundary.shp"),
  quiet = TRUE
)

make_yield_effects <- function(yield_data) {
  yield_data %>%
    select(crop, Basin, ISO_A3, state, value) %>%
    pivot_wider(names_from = state, values_from = value) %>%
    filter(if_all(all_of(names(scenario_states("global2"))), is.finite)) %>%
    transmute(
      crop,
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
    pivot_longer(
      -c(crop, Basin, ISO_A3),
      names_to = "contrast", values_to = "change_pct"
    ) %>%
    filter(is.finite(change_pct)) %>%
    mutate(contrast = factor(contrast, levels = c(
      "Protection without mitigation", "Protection under mitigation",
      "Protection interaction", "Afforestation contribution",
      "BECCS contribution", "Combined policy"
    )))
}

save_crop_maps <- function(effect_data, source_dir) {
  crop_groups <- effect_data %>%
    group_by(crop) %>%
    group_split()

  for (crop_data in crop_groups) {
    crop_code <- unique(crop_data$crop)
    crop_label <- unname(commodity_labels[crop_code])
    crop_stub <- paste0(
      crop_code, "_", gsub("[^a-z0-9]+", "_", tolower(crop_label))
    )
    crop_dir <- file.path(commodity_root, source_dir, crop_stub)
    dir.create(crop_dir, recursive = TRUE, showWarnings = FALSE)

    map_data <- basins %>%
      inner_join(crop_data, by = c("Basin", "ISO_A3"))
    fill_limit <- symmetric_limit(crop_data$change_pct)

    map_plot <- ggplot() +
      geom_sf(data = basins, fill = "grey92", colour = NA) +
      geom_sf(data = map_data, aes(fill = change_pct), colour = NA) +
      facet_wrap(vars(contrast), ncol = 3) +
      scale_fill_gradient2(
        low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
        midpoint = 0, limits = c(-fill_limit, fill_limit),
        oob = squish, name = "%"
      ) +
      coord_sf(expand = FALSE) +
      theme_void(base_size = 9) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(1.5, "mm")
      )

    ggsave(
      file.path(crop_dir, paste0("yield_effects_", target_year, ".png")),
      map_plot, width = 13.5, height = 6.8, dpi = 300, bg = "white"
    )
    write_csv(
      crop_data,
      file.path(crop_dir, paste0("yield_effects_", target_year, ".csv"))
    )
  }
}

# ALPHA yldBas_load, LandInt_global2 -----------------------------------------

alpha_scenarios <- scenario_states("global2")
alpha_effects <- rgdx.param(
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
    crop = as.character(c),
    Basin = as.character(bas),
    ISO_A3 = as.character(cty),
    value = yldBas_load
  ) %>%
  make_yield_effects()

save_crop_maps(alpha_effects, "alpha_yldBas")
write_csv(
  alpha_effects,
  file.path(commodity_root, paste0("alpha_yldBas_all_crops_", target_year, ".csv"))
)

rm(alpha_effects)
gc()

# PLUM yldPlum, LandInt_global1 ----------------------------------------------

plum_scenarios <- scenario_states("global1")
plum_effects <- rgdx.param(
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
    crop = as.character(a_cro),
    Basin = as.character(bas),
    ISO_A3 = as.character(cty),
    value = yldPlum
  ) %>%
  make_yield_effects()

save_crop_maps(plum_effects, "plum_yldPlum")
write_csv(
  plum_effects,
  file.path(commodity_root, paste0("plum_yldPlum_all_crops_", target_year, ".csv"))
)
