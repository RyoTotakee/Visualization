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

analysis_root <- file.path(
  output_dir, "07_conditional_protection_decomposition", "01_agriculture"
)
unlink(analysis_root, recursive = TRUE)
country_root <- file.path(analysis_root, "country")
region_root <- file.path(analysis_root, "region_r10")
world_root <- file.path(analysis_root, "world")
dir.create(country_root, recursive = TRUE, showWarnings = FALSE)
dir.create(region_root, recursive = TRUE, showWarnings = FALSE)
dir.create(world_root, recursive = TRUE, showWarnings = FALSE)

r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)
component_columns <- c(
  "protection_main", "protection_x_afforestation", "protection_x_beccs",
  "protection_x_afforestation_x_beccs"
)
component_labels <- c(
  protection_main = "Protection main effect",
  protection_x_afforestation = "Protection × afforestation",
  protection_x_beccs = "Protection × BECCS",
  protection_x_afforestation_x_beccs = "Three-way interaction"
)
component_colours <- c(
  "Protection main effect" = "#756bb1",
  "Protection × afforestation" = "#31a354",
  "Protection × BECCS" = "#e6550d",
  "Three-way interaction" = "#3182bd"
)

decompose_market <- function(data, geography) {
  result <- data %>%
    select(all_of(geography), item, item_label, item_group, is_aggregate,
           indicator, unit, state, value) %>%
    pivot_wider(names_from = state, values_from = value) %>%
    filter(if_all(all_of(names(scenario_states("global2"))), is.finite)) %>%
    filter(abs(`011`) > 1e-10) %>%
    conditional_protection_factorial() %>%
    mutate(
      across(
        all_of(c(component_columns, "protection_under_mitigation")),
        ~ 100 * .x / mitigation_baseline,
        .names = "{.col}_pct"
      )
    )
  if (nrow(result) > 0L &&
      max(abs(result$decomposition_residual), na.rm = TRUE) > 1e-7) {
    stop("Conditional protection decomposition did not close.")
  }
  result
}

component_long <- function(data, geography) {
  data %>%
    select(all_of(geography), all_of(paste0(component_columns, "_pct"))) %>%
    pivot_longer(-all_of(geography), names_to = "component", values_to = "effect_pct") %>%
    mutate(
      component = sub("_pct$", "", component),
      component = factor(unname(component_labels[component]),
                         levels = unname(component_labels))
    )
}

country_decomposition <- decompose_market(read_agmip_market("country"), "country")
aggregate_decomposition <- decompose_market(
  read_agmip_market("aggregate") %>%
    filter(region %in% c(names(r10_labels), "World")), "region"
)
write_csv(
  country_decomposition,
  file.path(country_root,
            paste0("country_conditional_protection_decomposition_", target_year, ".csv"))
)
write_csv(
  aggregate_decomposition %>% filter(region %in% names(r10_labels)),
  file.path(region_root,
            paste0("r10_conditional_protection_decomposition_", target_year, ".csv"))
)
write_csv(
  aggregate_decomposition %>% filter(region == "World"),
  file.path(world_root,
            paste0("world_conditional_protection_decomposition_", target_year, ".csv"))
)
write_csv(agmip_item_catalog, file.path(analysis_root, "agmip_item_selection.csv"))

item_directory <- function(root, item, label, group, aggregate) {
  stub <- paste0(tolower(gsub("[^A-Za-z0-9]+", "_", item)), "_",
                 tolower(gsub("[^A-Za-z0-9]+", "_", label)))
  file.path(root, if (aggregate) "aggregate_agmip_items" else "by_agmip_item",
            if (aggregate) stub else file.path(group, stub))
}

world_map <- ne_countries(scale = "small", returnclass = "sf") %>%
  select(country = iso_a3)
for (plot_data in group_split(group_by(
  country_decomposition, item, item_label, item_group, is_aggregate, indicator, unit
))) {
  item <- unique(plot_data$item); label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group); aggregate <- unique(plot_data$is_aggregate)
  indicator <- as.character(unique(plot_data$indicator))
  stub <- tolower(gsub("[^A-Za-z0-9]+", "_", indicator))
  plot_dir <- item_directory(country_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  map_values <- bind_rows(
    component_long(plot_data, "country") %>%
      transmute(country, panel = as.character(component), effect_pct),
    plot_data %>% transmute(country, panel = "Total under mitigation",
                            effect_pct = protection_under_mitigation_pct)
  ) %>%
    mutate(panel = factor(panel, levels = c(unname(component_labels),
                                            "Total under mitigation")))
  mapped <- world_map %>% inner_join(map_values, by = "country")
  limit <- symmetric_limit(map_values$effect_pct)
  p <- ggplot() +
    geom_sf(data = world_map, fill = "grey92", colour = "grey65", linewidth = 0.1) +
    geom_sf(data = mapped, aes(fill = effect_pct), colour = "grey65", linewidth = 0.1) +
    facet_wrap(vars(panel), ncol = 2) +
    scale_fill_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
                         midpoint = 0, limits = c(-limit, limit), oob = squish,
                         name = "% of mitigation\nbaseline") +
    coord_sf(expand = FALSE) + theme_void(base_size = 10) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(plot_dir, paste0(stub, "_component_maps_", target_year, ".png")),
         p, width = 11, height = 11, dpi = 300, bg = "white")

  top <- plot_data %>%
    slice_max(abs(protection_under_mitigation_pct), n = 12, with_ties = FALSE) %>%
    arrange(protection_under_mitigation_pct) %>%
    mutate(country = factor(country, levels = country))
  bars <- component_long(top, "country")
  bp <- ggplot(bars, aes(country, effect_pct, fill = component)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.7) +
    geom_point(data = top, aes(country, protection_under_mitigation_pct),
               inherit.aes = FALSE, shape = 18, size = 2.2) +
    coord_flip() + scale_fill_manual(values = component_colours, name = NULL) +
    labs(x = NULL, y = "Contribution (% of mitigation baseline)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0(stub, "_stacked_components_", target_year, ".png")),
         bp, width = 9, height = 6.4, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0(stub, "_components_", target_year, ".csv")))
}

for (plot_data in group_split(group_by(
  aggregate_decomposition %>% filter(region %in% names(r10_labels)),
  item, item_label, item_group, is_aggregate, indicator, unit
))) {
  item <- unique(plot_data$item); label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group); aggregate <- unique(plot_data$is_aggregate)
  indicator <- as.character(unique(plot_data$indicator))
  stub <- tolower(gsub("[^A-Za-z0-9]+", "_", indicator))
  plot_dir <- item_directory(region_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  plot_data <- plot_data %>%
    mutate(region_label = factor(unname(r10_labels[region]), levels = rev(unname(r10_labels))))
  bars <- component_long(plot_data, "region_label")
  p <- ggplot(bars, aes(region_label, effect_pct, fill = component)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.7) +
    geom_point(data = plot_data, aes(region_label, protection_under_mitigation_pct),
               inherit.aes = FALSE, shape = 18, size = 2.2) +
    coord_flip() + scale_fill_manual(values = component_colours, name = NULL) +
    labs(x = NULL, y = "Contribution (% of mitigation baseline)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0(stub, "_stacked_components_", target_year, ".png")),
         p, width = 9, height = 6.1, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0(stub, "_components_", target_year, ".csv")))
}

for (plot_data in group_split(group_by(
  aggregate_decomposition %>% filter(region == "World"),
  item, item_label, item_group, is_aggregate
))) {
  item <- unique(plot_data$item); label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group); aggregate <- unique(plot_data$is_aggregate)
  plot_dir <- item_directory(world_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  plot_data <- plot_data %>%
    mutate(indicator = factor(as.character(indicator), levels = agmip_indicator_order))
  bars <- component_long(plot_data, "indicator")
  p <- ggplot(bars, aes(indicator, effect_pct, fill = component)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.7) +
    geom_point(data = plot_data, aes(indicator, protection_under_mitigation_pct),
               inherit.aes = FALSE, shape = 18, size = 2.2) +
    coord_flip() + scale_fill_manual(values = component_colours, name = NULL) +
    labs(x = NULL, y = "Contribution (% of mitigation baseline)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0("world_stacked_components_", target_year, ".png")),
         p, width = 9, height = 6.1, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0("world_components_", target_year, ".csv")))
}
