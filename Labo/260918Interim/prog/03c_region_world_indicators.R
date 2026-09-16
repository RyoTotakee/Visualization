suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

agriculture_root <- file.path(output_dir, "03_country_indicators")
region_root <- file.path(agriculture_root, "region", "r10")
world_root <- file.path(agriculture_root, "world")
unlink(file.path(agriculture_root, "region"), recursive = TRUE)
unlink(world_root, recursive = TRUE)
dir.create(region_root, recursive = TRUE, showWarnings = FALSE)
dir.create(world_root, recursive = TRUE, showWarnings = FALSE)

r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)
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

market <- read_agmip_market("aggregate") %>%
  filter(region %in% c(names(r10_labels), "World"))

market_shapley <- market %>%
  select(region, item, item_label, item_group, is_aggregate,
         indicator, unit, state, value) %>%
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

r10_shapley <- market_shapley %>% filter(region %in% names(r10_labels))
world_shapley <- market_shapley %>% filter(region == "World")
write_csv(
  r10_shapley,
  file.path(region_root, paste0("r10_agmip_market_shapley_", target_year, ".csv"))
)
write_csv(
  world_shapley,
  file.path(world_root, paste0("world_agmip_market_shapley_", target_year, ".csv"))
)
write_csv(agmip_item_catalog, file.path(region_root, "agmip_item_selection.csv"))
write_csv(agmip_item_catalog, file.path(world_root, "agmip_item_selection.csv"))

item_directory <- function(root, item, label, group, aggregate) {
  item_stub <- paste0(
    tolower(gsub("[^A-Za-z0-9]+", "_", item)), "_",
    tolower(gsub("[^A-Za-z0-9]+", "_", label))
  )
  file.path(
    root,
    if (aggregate) "aggregate_agmip_items" else "by_agmip_item",
    if (aggregate) item_stub else file.path(group, item_stub)
  )
}

# R10: one contribution chart and one context heat map per item and indicator.
for (plot_data in group_split(
  group_by(r10_shapley, item, item_label, item_group, is_aggregate,
           indicator, unit)
)) {
  item <- unique(plot_data$item)
  label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group)
  aggregate <- unique(plot_data$is_aggregate)
  indicator <- as.character(unique(plot_data$indicator))
  unit <- unique(plot_data$unit)
  stub <- tolower(gsub("[^A-Za-z0-9]+", "_", indicator))
  plot_dir <- item_directory(region_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  plot_data <- plot_data %>%
    mutate(region_label = factor(
      unname(r10_labels[region]), levels = rev(unname(r10_labels))
    ))

  physical <- indicator %in% c(
    "Area", "Production", "Consumption", "Exports", "Imports", "Net exports"
  )
  if (physical) {
    bars <- plot_data %>%
      select(region_label, protection, afforestation, beccs) %>%
      pivot_longer(-region_label, names_to = "policy", values_to = "contribution") %>%
      mutate(policy = factor(raw_policy_labels[policy], levels = raw_policy_labels))
    points <- plot_data %>% mutate(total_plot = total)
    axis_label <- paste0("Contribution (", unit, ")")
  } else {
    bars <- plot_data %>%
      select(region_label, all_of(names(policy_labels))) %>%
      pivot_longer(-region_label, names_to = "policy", values_to = "contribution") %>%
      mutate(policy = factor(policy_labels[policy], levels = policy_labels))
    points <- plot_data %>% mutate(total_plot = total_pct)
    axis_label <- "Contribution (% of BaU)"
  }

  p <- ggplot(bars, aes(region_label, contribution, fill = policy)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.72) +
    geom_point(data = points, aes(region_label, total_plot), inherit.aes = FALSE,
               shape = 18, size = 2.2) +
    coord_flip() +
    scale_fill_manual(values = c("#756bb1", "#31a354", "#e6550d"), name = NULL) +
    labs(x = NULL, y = axis_label) +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0(stub, "_shapley_bars_", target_year, ".png")),
         p, width = 8.5, height = 6.2, dpi = 300, bg = "white")

  heat <- plot_data %>%
    select(region_label, all_of(names(policy_labels)), total_pct) %>%
    pivot_longer(-region_label, names_to = "component", values_to = "effect_pct") %>%
    mutate(component = factor(
      recode(component, !!!policy_labels, total_pct = "Total"),
      levels = c(unname(policy_labels), "Total")
    ))
  limit <- symmetric_limit(heat$effect_pct)
  hp <- ggplot(heat, aes(component, region_label, fill = effect_pct)) +
    geom_tile(colour = "white", linewidth = 0.25) +
    scale_fill_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
                         midpoint = 0, limits = c(-limit, limit), oob = squish,
                         name = "% of BaU") +
    labs(x = NULL, y = NULL) + theme_paper() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1))
  ggsave(file.path(plot_dir, paste0(stub, "_shapley_heatmap_", target_year, ".png")),
         hp, width = 7.2, height = 5.5, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0(stub, "_", target_year, ".csv")))
}

# World: percentage contributions across all available market indicators.
for (plot_data in group_split(
  group_by(world_shapley, item, item_label, item_group, is_aggregate)
)) {
  item <- unique(plot_data$item)
  label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group)
  aggregate <- unique(plot_data$is_aggregate)
  plot_dir <- item_directory(world_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  bars <- plot_data %>%
    mutate(indicator = factor(as.character(indicator), levels = agmip_indicator_order)) %>%
    select(indicator, all_of(names(policy_labels))) %>%
    pivot_longer(-indicator, names_to = "policy", values_to = "contribution_pct") %>%
    mutate(policy = factor(policy_labels[policy], levels = policy_labels))
  p <- ggplot(bars, aes(indicator, contribution_pct, fill = policy)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.7) +
    geom_point(data = plot_data,
               aes(factor(as.character(indicator), levels = agmip_indicator_order), total_pct),
               inherit.aes = FALSE, shape = 18, size = 2.2) +
    coord_flip() +
    scale_fill_manual(values = c("#756bb1", "#31a354", "#e6550d"), name = NULL) +
    labs(x = NULL, y = "Contribution (% of BaU)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0("world_shapley_overview_", target_year, ".png")),
         p, width = 8.5, height = 6.0, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0("world_market_", target_year, ".csv")))
}
