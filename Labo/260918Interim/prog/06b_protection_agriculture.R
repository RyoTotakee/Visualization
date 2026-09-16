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

analysis_root <- file.path(output_dir, "06_protection_focus", "03_agriculture")
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

protection_effects <- function(data, geography) {
  data %>%
    select(all_of(geography), item, item_label, item_group, is_aggregate,
           indicator, unit, state, value) %>%
    pivot_wider(names_from = state, values_from = value) %>%
    filter(if_all(all_of(c("000", "100", "011", "111")), is.finite)) %>%
    filter(abs(`000`) > 1e-10, abs(`011`) > 1e-10) %>%
    mutate(
      protection_without_mitigation = `100` - `000`,
      protection_under_mitigation = `111` - `011`,
      protection_interaction = protection_under_mitigation - protection_without_mitigation,
      protection_without_mitigation_pct = 100 * protection_without_mitigation / `000`,
      protection_under_mitigation_pct = 100 * protection_under_mitigation / `011`,
      protection_interaction_pct_of_bau = 100 * protection_interaction / `000`,
      protection_context_difference_pp = protection_under_mitigation_pct -
        protection_without_mitigation_pct
    )
}

country_effects <- protection_effects(read_agmip_market("country"), "country")
aggregate_effects <- protection_effects(
  read_agmip_market("aggregate") %>%
    filter(region %in% c(names(r10_labels), "World")), "region"
)
write_csv(country_effects,
          file.path(country_root, paste0("country_agmip_protection_effects_", target_year, ".csv")))
write_csv(aggregate_effects %>% filter(region %in% names(r10_labels)),
          file.path(region_root, paste0("r10_agmip_protection_effects_", target_year, ".csv")))
write_csv(aggregate_effects %>% filter(region == "World"),
          file.path(world_root, paste0("world_agmip_protection_effects_", target_year, ".csv")))
write_csv(agmip_item_catalog, file.path(analysis_root, "agmip_item_selection.csv"))

item_directory <- function(root, item, label, group, aggregate) {
  stub <- paste0(tolower(gsub("[^A-Za-z0-9]+", "_", item)), "_",
                 tolower(gsub("[^A-Za-z0-9]+", "_", label)))
  file.path(root, if (aggregate) "aggregate_agmip_items" else "by_agmip_item",
            if (aggregate) stub else file.path(group, stub))
}

context_columns <- c(
  protection_without_mitigation_pct = "Without mitigation",
  protection_under_mitigation_pct = "Under mitigation",
  protection_context_difference_pp = "Context difference"
)
world_map <- ne_countries(scale = "small", returnclass = "sf") %>%
  select(country = iso_a3)

for (plot_data in group_split(group_by(
  country_effects, item, item_label, item_group, is_aggregate, indicator, unit
))) {
  item <- unique(plot_data$item); label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group); aggregate <- unique(plot_data$is_aggregate)
  indicator <- as.character(unique(plot_data$indicator))
  stub <- tolower(gsub("[^A-Za-z0-9]+", "_", indicator))
  plot_dir <- item_directory(country_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  values <- plot_data %>%
    select(country, all_of(names(context_columns))) %>%
    pivot_longer(-country, names_to = "context", values_to = "effect_pct") %>%
    mutate(context = factor(context_columns[context], levels = context_columns))
  mapped <- world_map %>% inner_join(values, by = "country")
  limit <- symmetric_limit(values$effect_pct)
  p <- ggplot() +
    geom_sf(data = world_map, fill = "grey92", colour = "grey65", linewidth = 0.1) +
    geom_sf(data = mapped, aes(fill = effect_pct), colour = "grey65", linewidth = 0.1) +
    facet_wrap(vars(context), ncol = 1) +
    scale_fill_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
                         midpoint = 0, limits = c(-limit, limit), oob = squish,
                         name = "% or pp") +
    coord_sf(expand = FALSE) + theme_void(base_size = 10) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
  ggsave(file.path(plot_dir, paste0(stub, "_protection_context_maps_", target_year, ".png")),
         p, width = 9, height = 9, dpi = 300, bg = "white")

  top <- plot_data %>%
    slice_max(abs(protection_under_mitigation_pct), n = 12, with_ties = FALSE) %>%
    arrange(protection_under_mitigation_pct) %>%
    mutate(country = factor(country, levels = country))
  bars <- top %>% select(country, all_of(names(context_columns))) %>%
    pivot_longer(-country, names_to = "context", values_to = "effect_pct") %>%
    mutate(context = factor(context_columns[context], levels = context_columns))
  bp <- ggplot(bars, aes(country, effect_pct, fill = context)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(position = position_dodge(width = 0.76), width = 0.68) + coord_flip() +
    scale_fill_manual(values = c("#807dba", "#238b45", "#e6550d"), name = NULL) +
    labs(x = NULL, y = "Protection effect (%); context difference (pp)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0(stub, "_protection_context_bars_", target_year, ".png")),
         bp, width = 8.8, height = 6.3, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0(stub, "_", target_year, ".csv")))
}

for (plot_data in group_split(group_by(
  aggregate_effects %>% filter(region %in% names(r10_labels)),
  item, item_label, item_group, is_aggregate, indicator, unit
))) {
  item <- unique(plot_data$item); label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group); aggregate <- unique(plot_data$is_aggregate)
  indicator <- as.character(unique(plot_data$indicator))
  stub <- tolower(gsub("[^A-Za-z0-9]+", "_", indicator))
  plot_dir <- item_directory(region_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  bars <- plot_data %>%
    mutate(region_label = factor(unname(r10_labels[region]), levels = rev(unname(r10_labels)))) %>%
    select(region_label, all_of(names(context_columns))) %>%
    pivot_longer(-region_label, names_to = "context", values_to = "effect_pct") %>%
    mutate(context = factor(context_columns[context], levels = context_columns))
  p <- ggplot(bars, aes(region_label, effect_pct, fill = context)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(position = position_dodge(width = 0.76), width = 0.68) + coord_flip() +
    scale_fill_manual(values = c("#807dba", "#238b45", "#e6550d"), name = NULL) +
    labs(x = NULL, y = "Protection effect (%); context difference (pp)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0(stub, "_protection_context_", target_year, ".png")),
         p, width = 8.8, height = 6.0, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0(stub, "_", target_year, ".csv")))
}

for (plot_data in group_split(group_by(
  aggregate_effects %>% filter(region == "World"),
  item, item_label, item_group, is_aggregate
))) {
  item <- unique(plot_data$item); label <- unique(plot_data$item_label)
  group <- unique(plot_data$item_group); aggregate <- unique(plot_data$is_aggregate)
  plot_dir <- item_directory(world_root, item, label, group, aggregate)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  bars <- plot_data %>%
    mutate(indicator = factor(as.character(indicator), levels = agmip_indicator_order)) %>%
    select(indicator, all_of(names(context_columns))) %>%
    pivot_longer(-indicator, names_to = "context", values_to = "effect_pct") %>%
    mutate(context = factor(context_columns[context], levels = context_columns))
  p <- ggplot(bars, aes(indicator, effect_pct, fill = context)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(position = position_dodge(width = 0.76), width = 0.68) + coord_flip() +
    scale_fill_manual(values = c("#807dba", "#238b45", "#e6550d"), name = NULL) +
    labs(x = NULL, y = "Protection effect (%); context difference (pp)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0("world_protection_context_", target_year, ".png")),
         p, width = 8.8, height = 6.0, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0("world_market_", target_year, ".csv")))
}
