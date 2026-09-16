suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

item_output_dir <- file.path(
  output_dir, "03_country_indicators", "country", "by_agmip_item"
)
dir.create(item_output_dir, recursive = TRUE, showWarnings = FALSE)

# Country-level detailed AgMIP items ----------------------------------------

country_market <- read_agmip_market("country") %>%
  filter(!is_aggregate)

country_item_shapley <- country_market %>%
  select(
    country, item, item_label, item_group, indicator, unit, state, value
  ) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  filter(if_all(all_of(names(scenario_states("global2"))), is.finite)) %>%
  shapley_three() %>%
  filter(abs(baseline) > 1e-10) %>%
  mutate(
    protection_pct = 100 * protection / baseline,
    afforestation_pct = 100 * afforestation / baseline,
    beccs_pct = 100 * beccs / baseline,
    total_pct = 100 * total / baseline
  )

write_csv(
  agmip_item_catalog,
  file.path(item_output_dir, "agmip_item_selection.csv")
)
write_csv(
  country_item_shapley,
  file.path(
    item_output_dir, paste0("country_agmip_item_shapley_", target_year, ".csv")
  )
)

world <- ne_countries(scale = "small", returnclass = "sf") %>%
  select(country = iso_a3)
policy_labels <- c(
  protection_pct = "Protected areas",
  afforestation_pct = "Afforestation",
  beccs_pct = "BECCS"
)
raw_policy_labels <- c(
  protection = "Protected areas",
  afforestation = "Afforestation",
  beccs = "BECCS"
)

plot_groups <- country_item_shapley %>%
  group_by(item, item_label, item_group, indicator, unit) %>%
  group_split()

for (indicator_data in plot_groups) {
  item_name <- unique(indicator_data$item)
  item_label <- unique(indicator_data$item_label)
  item_group <- unique(indicator_data$item_group)
  indicator_name <- as.character(unique(indicator_data$indicator))
  item_stub <- paste0(
    tolower(gsub("[^A-Za-z0-9]+", "_", item_name)), "_",
    tolower(gsub("[^A-Za-z0-9]+", "_", item_label))
  )
  indicator_stub <- tolower(gsub("[^A-Za-z0-9]+", "_", indicator_name))
  plot_dir <- file.path(item_output_dir, item_group, item_stub)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  map_values <- indicator_data %>%
    select(country, all_of(names(policy_labels))) %>%
    pivot_longer(
      all_of(names(policy_labels)), names_to = "policy",
      values_to = "contribution_pct"
    ) %>%
    mutate(policy = factor(policy_labels[policy], levels = policy_labels))
  map_data <- world %>% inner_join(map_values, by = "country")
  map_limit <- symmetric_limit(map_values$contribution_pct)

  item_map <- ggplot() +
    geom_sf(data = world, fill = "grey92", colour = "grey65", linewidth = 0.1) +
    geom_sf(
      data = map_data, aes(fill = contribution_pct),
      colour = "grey65", linewidth = 0.1
    ) +
    facet_wrap(vars(policy), ncol = 1) +
    scale_fill_gradient2(
      low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
      midpoint = 0, limits = c(-map_limit, map_limit), oob = squish,
      name = "% of BaU"
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

  ggsave(
    file.path(plot_dir, paste0(indicator_stub, "_shapley_maps_", target_year, ".png")),
    item_map, width = 10.5, height = 10.5, dpi = 300, bg = "white"
  )

  physical_indicator <- indicator_name %in% c(
    "Area", "Production", "Consumption", "Exports", "Imports", "Net exports"
  )
  ranking_value <- if (physical_indicator) {
    abs(indicator_data$total)
  } else {
    abs(indicator_data$total_pct)
  }
  top_countries <- indicator_data %>%
    mutate(ranking_value = ranking_value) %>%
    slice_max(ranking_value, n = 12, with_ties = FALSE) %>%
    arrange(total_pct) %>%
    mutate(country = factor(country, levels = country))

  if (physical_indicator) {
    bar_values <- top_countries %>%
      select(country, protection, afforestation, beccs) %>%
      pivot_longer(
        c(protection, afforestation, beccs),
        names_to = "policy", values_to = "contribution"
      ) %>%
      mutate(policy = factor(raw_policy_labels[policy], levels = raw_policy_labels))
    point_values <- top_countries %>% mutate(total_plot = total)
    y_label <- paste0("Contribution (", unique(top_countries$unit), ")")
  } else {
    bar_values <- top_countries %>%
      select(country, all_of(names(policy_labels))) %>%
      pivot_longer(
        all_of(names(policy_labels)), names_to = "policy",
        values_to = "contribution"
      ) %>%
      mutate(policy = factor(policy_labels[policy], levels = policy_labels))
    point_values <- top_countries %>% mutate(total_plot = total_pct)
    y_label <- "Contribution (% of BaU)"
  }

  item_bar <- ggplot(bar_values, aes(country, contribution, fill = policy)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.72) +
    geom_point(
      data = point_values, aes(country, total_plot), inherit.aes = FALSE,
      shape = 18, size = 2.2, colour = "black"
    ) +
    coord_flip() +
    scale_fill_manual(values = c("#756bb1", "#31a354", "#e6550d"), name = NULL) +
    labs(x = NULL, y = y_label) +
    theme_paper() +
    theme(panel.grid.major.y = element_blank())

  ggsave(
    file.path(plot_dir, paste0(indicator_stub, "_shapley_bars_", target_year, ".png")),
    item_bar, width = 8.5, height = 6.5, dpi = 300, bg = "white"
  )
  write_csv(
    indicator_data,
    file.path(plot_dir, paste0(indicator_stub, "_", target_year, ".csv"))
  )
}
