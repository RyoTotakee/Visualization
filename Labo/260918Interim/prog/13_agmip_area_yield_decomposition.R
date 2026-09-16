suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

analysis_root <- file.path(output_dir, "13_agmip_area_yield_decomposition")
unlink(analysis_root, recursive = TRUE)
world_root <- file.path(analysis_root, "01_world")
r10_root <- file.path(analysis_root, "02_r10")
country_root <- file.path(analysis_root, "03_country")
bridge_root <- file.path(analysis_root, "04_land_cover_vs_harvested_area")
for (path in c(world_root, r10_root, country_root, bridge_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

decomp_theme <- theme_paper() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 8)),
    plot.caption = element_text(size = 8.3, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

component_colours <- c(
  "Harvested-area contribution" = "#E69F00",
  "Effective-yield contribution" = "#0072B2"
)
r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)
comparison_table <- tribble(
  ~comparison, ~from, ~to, ~comparison_label,
  "protection_no_cdr", "000", "100", "Protection\nwithout CDR",
  "afforestation", "000", "010", "Afforestation",
  "beccs", "000", "001", "BECCS",
  "both_cdr", "000", "011", "Both CDR",
  "protection_with_cdr", "011", "111", "Protection\nwith both CDR",
  "all_policies", "000", "111", "All policies"
)

# Symmetric two-factor decomposition ----------------------------------------

# For P = A * Y, this split assigns half of the A-Y cross term to each
# component. It is exact and does not depend on whether area or yield is
# changed first:
#   area  = (A1-A0) * (Y0+Y1)/2
#   yield = (Y1-Y0) * (A0+A1)/2
decompose_area_yield <- function(market_data, id_columns, comparisons) {
  market_wide <- market_data %>%
    filter(indicator %in% c("Area", "Production")) %>%
    mutate(indicator = as.character(indicator)) %>%
    select(all_of(id_columns), state, indicator, value) %>%
    pivot_wider(
      names_from = c(indicator, state), values_from = value,
      names_glue = "{indicator}_{state}"
    )

  result <- vector("list", nrow(comparisons))
  for (i in seq_len(nrow(comparisons))) {
    from <- comparisons$from[[i]]
    to <- comparisons$to[[i]]
    result[[i]] <- market_wide %>%
      transmute(
        across(all_of(id_columns)),
        comparison = comparisons$comparison[[i]],
        comparison_label = comparisons$comparison_label[[i]],
        from = from, to = to,
        area_from_1000ha = .data[[paste0("Area_", from)]],
        area_to_1000ha = .data[[paste0("Area_", to)]],
        production_from_pj = .data[[paste0("Production_", from)]],
        production_to_pj = .data[[paste0("Production_", to)]]
      ) %>%
      filter(
        is.finite(area_from_1000ha), is.finite(area_to_1000ha),
        is.finite(production_from_pj), is.finite(production_to_pj),
        area_from_1000ha > 1e-12, area_to_1000ha > 1e-12,
        production_from_pj > 1e-12
      ) %>%
      mutate(
        effective_yield_from_gj_ha = 1000 * production_from_pj / area_from_1000ha,
        effective_yield_to_gj_ha = 1000 * production_to_pj / area_to_1000ha,
        area_effect_pj =
          (area_to_1000ha - area_from_1000ha) *
          (effective_yield_from_gj_ha + effective_yield_to_gj_ha) / 2000,
        yield_effect_pj =
          (effective_yield_to_gj_ha - effective_yield_from_gj_ha) *
          (area_from_1000ha + area_to_1000ha) / 2000,
        total_change_pj = production_to_pj - production_from_pj,
        closure_pj = total_change_pj - area_effect_pj - yield_effect_pj,
        area_change_pct = 100 * (area_to_1000ha / area_from_1000ha - 1),
        effective_yield_change_pct = 100 *
          (effective_yield_to_gj_ha / effective_yield_from_gj_ha - 1),
        production_change_pct = 100 *
          (production_to_pj / production_from_pj - 1),
        area_effect_pp = 100 * area_effect_pj / production_from_pj,
        yield_effect_pp = 100 * yield_effect_pj / production_from_pj,
        total_change_pct_from_components = area_effect_pp + yield_effect_pp
      )
  }
  bind_rows(result)
}

make_component_long <- function(x) {
  x %>%
    pivot_longer(
      c(area_effect_pp, yield_effect_pp),
      names_to = "component", values_to = "contribution_pp"
    ) %>%
    mutate(
      component = recode(
        component,
        area_effect_pp = "Harvested-area contribution",
        yield_effect_pp = "Effective-yield contribution"
      ),
      component = factor(component, levels = names(component_colours))
    )
}

# 1. World and R10 at the selected year ------------------------------------

agmip_aggregate <- read_agmip_market("aggregate")
world_decomposition <- agmip_aggregate %>%
  filter(region == "World") %>%
  decompose_area_yield(c("region", "item", "item_label"), comparison_table)
r10_decomposition <- agmip_aggregate %>%
  filter(region %in% names(r10_labels)) %>%
  decompose_area_yield(c("region", "item", "item_label"), comparison_table)

write_csv(world_decomposition,
          file.path(world_root, paste0("world_area_yield_decomposition_", target_year, ".csv")))
write_csv(r10_decomposition,
          file.path(r10_root, paste0("r10_area_yield_decomposition_", target_year, ".csv")))

world_crp <- world_decomposition %>%
  filter(item == "CRP") %>%
  mutate(
    comparison_label = factor(
      comparison_label,
      levels = comparison_table$comparison_label
    )
  )
world_crp_long <- make_component_long(world_crp)
p <- ggplot(world_crp_long, aes(comparison_label, contribution_pp, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .35) +
  geom_col(width = .7) +
  geom_point(
    data = world_crp,
    aes(comparison_label, production_change_pct, shape = "Total production change"),
    inherit.aes = FALSE, size = 3.1, colour = "black"
  ) +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(
    title = "Why global crop production changes: harvested area or effective yield?",
    subtitle = "The two coloured contributions add exactly to the black total-production marker",
    x = NULL, y = "Contribution to CRP production change (percentage points)",
    caption = paste0(
      "AgMIP CRP (non-energy crops), ", target_year,
      ". Effective caloric yield = production (PJ/yr) / harvested area. The symmetric split assigns half of the area-yield cross term to each component."
    )
  ) + decomp_theme +
  theme(axis.text.x = element_text(angle = 18, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(world_root, paste0("world_crp_area_yield_decomposition_", target_year, ".png")),
       p, width = 11.5, height = 6.8, dpi = 300, bg = "white")

major_items <- c("WHT", "RIC", "CGR", "OSD", "SGC")
world_major <- world_decomposition %>%
  filter(item %in% major_items, comparison == "protection_with_cdr") %>%
  mutate(item_label = factor(item_label, levels = item_label[order(total_change_pct_from_components)]))
world_major_long <- make_component_long(world_major)
p <- ggplot(world_major_long, aes(item_label, contribution_pp, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .35) +
  geom_col(width = .68) +
  geom_point(
    data = world_major,
    aes(item_label, production_change_pct, shape = "Total production change"),
    inherit.aes = FALSE, size = 3.1, colour = "black"
  ) +
  coord_flip() +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(
    title = "Protection affects major crops through different area and yield responses",
    subtitle = "Protection is added to the scenario already containing afforestation and BECCS (111 minus 011)",
    x = NULL, y = "Contribution to production change (percentage points)",
    caption = paste0(
      "AgMIP, ", target_year,
      ". Energy crops are excluded. Effective yield is caloric production per harvested hectare; the black diamond is the exact production change."
    )
  ) + decomp_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(world_root, paste0("world_major_crop_area_yield_decomposition_", target_year, ".png")),
       p, width = 10.5, height = 6.3, dpi = 300, bg = "white")

r10_crp <- r10_decomposition %>%
  filter(item == "CRP", comparison == "protection_with_cdr") %>%
  mutate(
    region_label = unname(r10_labels[region]),
    region_label = factor(region_label, levels = region_label[order(total_change_pct_from_components)])
  )
r10_crp_long <- make_component_long(r10_crp)
p <- ggplot(r10_crp_long, aes(region_label, contribution_pp, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .35) +
  geom_col(width = .69) +
  geom_point(
    data = r10_crp,
    aes(region_label, production_change_pct, shape = "Total production change"),
    inherit.aes = FALSE, size = 2.9, colour = "black"
  ) +
  coord_flip() +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "The production response to protection differs sharply across regions",
    subtitle = "Area and effective-yield contributions to AgMIP CRP production, 111 minus 011",
    x = NULL, y = "Contribution to production change (percentage points)",
    caption = paste0(
      "AgMIP R10 direct aggregates, ", target_year,
      ". Positive harvested-area contribution means that expanded harvested area supports production; it does not imply higher land productivity."
    )
  ) + decomp_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(r10_root, paste0("r10_crp_area_yield_decomposition_", target_year, ".png")),
       p, width = 10.8, height = 7.4, dpi = 300, bg = "white")

# 2. Countries --------------------------------------------------------------

agmip_country <- read_agmip_market("country")
country_decomposition <- agmip_country %>%
  filter(item == "CRP") %>%
  decompose_area_yield(c("country", "item", "item_label"), comparison_table) %>%
  mutate(abs_total_change_pj = abs(total_change_pj))
write_csv(country_decomposition,
          file.path(country_root, paste0("country_crp_area_yield_decomposition_", target_year, ".csv")))

country_top <- country_decomposition %>%
  filter(comparison == "protection_with_cdr") %>%
  slice_max(abs_total_change_pj, n = 25, with_ties = FALSE) %>%
  mutate(country = factor(country, levels = country[order(total_change_pj)]))
country_top_long <- country_top %>%
  pivot_longer(
    c(area_effect_pj, yield_effect_pj), names_to = "component", values_to = "contribution_pj"
  ) %>%
  mutate(
    component = recode(
      component,
      area_effect_pj = "Harvested-area contribution",
      yield_effect_pj = "Effective-yield contribution"
    ),
    component = factor(component, levels = names(component_colours))
  )
p <- ggplot(country_top_long, aes(country, contribution_pj, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .35) +
  geom_col(width = .68) +
  geom_point(
    data = country_top,
    aes(country, total_change_pj, shape = "Total production change"),
    inherit.aes = FALSE, size = 2.5, colour = "black"
  ) +
  coord_flip() +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(
    title = "Countries contributing most to the global crop-production response",
    subtitle = "Top 25 by absolute CRP production change when protection is added under both CDR options",
    x = NULL, y = "Contribution to production change (PJ/yr)",
    caption = paste0(
      "AgMIP country outputs, ", target_year,
      ". Selection uses absolute PJ change, avoiding the unstable ranking of small producers by percentage change.",
      "\nCountry codes follow the AgMIP file."
    )
  ) + decomp_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(country_root, paste0("country_crp_area_yield_decomposition_top25_", target_year, ".png")),
       p, width = 10.5, height = 9.2, dpi = 300, bg = "white")

# 3. Connect PLUM land cover to AgMIP harvested area -----------------------

land_displacement_file <- file.path(
  output_dir, "08_land_use_protection", "02_protection_displacement",
  "protection_land_displacement_all_levels.csv"
)
food_land <- read_csv(land_displacement_file, show_col_types = FALSE) %>%
  filter(year == target_year, land_class == "Food cropland")

world_chain <- world_crp %>%
  filter(comparison == "protection_with_cdr") %>%
  transmute(
    geography = "WLD",
    `Merged Food cropland` = NA_real_,
    `AgMIP harvested area` = area_change_pct,
    `AgMIP effective caloric yield` = effective_yield_change_pct,
    `AgMIP caloric production` = production_change_pct
  ) %>%
  left_join(
    food_land %>%
      filter(geography_level == "world") %>%
      select(geography, food_cropland_change_pct = protection_under_mitigation_pct),
    by = "geography"
  ) %>%
  mutate(`Merged Food cropland` = food_cropland_change_pct) %>%
  select(-food_cropland_change_pct) %>%
  pivot_longer(-geography, names_to = "metric", values_to = "change_pct") %>%
  mutate(metric = factor(metric, levels = c(
    "Merged Food cropland", "AgMIP harvested area",
    "AgMIP effective caloric yield", "AgMIP caloric production"
  )))
write_csv(world_chain,
          file.path(bridge_root, paste0("world_food_land_harvested_area_yield_chain_", target_year, ".csv")))

p <- ggplot(world_chain, aes(metric, change_pct, fill = change_pct > 0)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .4) +
  geom_col(width = .64) +
  geom_text(
    aes(label = number(change_pct, accuracy = .01, suffix = "%")),
    vjust = if_else(world_chain$change_pct >= 0, -0.45, 1.35), size = 3.7
  ) +
  scale_fill_manual(values = c(`TRUE` = "#E69F00", `FALSE` = "#0072B2"), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = .1),
                     expand = expansion(mult = c(.16, .18))) +
  labs(
    title = "More food cropland does not translate into more crop production",
    subtitle = "Global response when protection is added under afforestation and BECCS (111 minus 011)",
    x = NULL, y = "Change from the Both-CDR scenario (%)",
    caption = paste0(
      "MergedComparison supplies physical Food cropland; AgMIP supplies CRP harvested area and caloric production, ",
      "\n", target_year, ". The two area measures have different definitions and should not be equated."
    )
  ) + decomp_theme +
  theme(axis.text.x = element_text(angle = 18, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(bridge_root, paste0("world_food_land_harvested_area_yield_chain_", target_year, ".png")),
       p, width = 10.5, height = 6.5, dpi = 300, bg = "white")

r10_chain <- r10_crp %>%
  transmute(
    geography = region,
    region = unname(r10_labels[region]),
    `AgMIP harvested area` = area_change_pct,
    `AgMIP effective caloric yield` = effective_yield_change_pct,
    `AgMIP caloric production` = production_change_pct
  ) %>%
  left_join(
    food_land %>%
      filter(geography_level == "r10") %>%
      select(geography, `Merged Food cropland` = protection_under_mitigation_pct),
    by = "geography"
  ) %>%
  pivot_longer(
    c(`Merged Food cropland`, `AgMIP harvested area`,
      `AgMIP effective caloric yield`, `AgMIP caloric production`),
    names_to = "metric", values_to = "change_pct"
  ) %>%
  mutate(
    metric = factor(metric, levels = c(
      "Merged Food cropland", "AgMIP harvested area",
      "AgMIP effective caloric yield", "AgMIP caloric production"
    )),
    region = factor(region, levels = unname(r10_labels))
  )
write_csv(r10_chain,
          file.path(bridge_root, paste0("r10_food_land_harvested_area_yield_chain_", target_year, ".csv")))

p <- ggplot(r10_chain, aes(change_pct, region, colour = metric, shape = metric)) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = .4) +
  geom_line(aes(group = region), colour = "grey75", linewidth = .45) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c(
    "Merged Food cropland" = "#E69F00",
    "AgMIP harvested area" = "#CC79A7",
    "AgMIP effective caloric yield" = "#0072B2",
    "AgMIP caloric production" = "#000000"
  ), name = NULL) +
  scale_shape_manual(values = c(15, 17, 16, 18), name = NULL) +
  scale_x_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs(
    title = "Land, harvested area, yield and production follow different regional pathways",
    subtitle = "Protection under both CDR options (111 minus 011); each row is one R10 region",
    x = "Change from the Both-CDR scenario (%)", y = NULL,
    caption = paste0(
      "MergedComparison Food cropland and AgMIP CRP direct R10 aggregates, ", target_year,
      ". Lines only connect the four diagnostics within a region; they are not a time sequence."
    )
  ) + decomp_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(bridge_root, paste0("r10_food_land_harvested_area_yield_chain_", target_year, ".png")),
       p, width = 11.3, height = 7.2, dpi = 300, bg = "white")

# 4. World CRP decomposition through time ----------------------------------

agmip_file <- file.path(data_dir, "ALPHA", "AGMIP_all_scenario.gdx")
analysis_years <- seq(2020L, target_year, by = 10L)
if (!target_year %in% analysis_years) analysis_years <- sort(c(analysis_years, target_year))
time_request <- list(
  name = "AGMIP_agg", form = "full",
  uels = list(
    unname(scenario_states("global2")), "World", c("AREA", "PROD"), "CRP",
    c("1000 ha", "million kcal"), as.character(analysis_years)
  )
)
time_array <- rgdx(agmip_file, time_request, squeeze = FALSE, useDomInfo = TRUE)
time_raw <- as.data.frame.table(
  time_array$val, responseName = "value", stringsAsFactors = FALSE
)
names(time_raw) <- c("scenario", "region", "variable", "item", "unit", "year", "value")
time_market <- time_raw %>%
  mutate(
    state = state_from_scenario(scenario), year = as.integer(year),
    indicator = recode(variable, AREA = "Area", PROD = "Production"),
    value = if_else(variable == "PROD", as.numeric(value) * 4.184e-6, as.numeric(value)),
    item_label = "Crops (non-energy)"
  ) %>%
  filter(
    (variable == "AREA" & unit == "1000 ha") |
      (variable == "PROD" & unit == "million kcal")
  ) %>%
  select(region, year, item, item_label, state, indicator, value)
world_time <- decompose_area_yield(
  time_market,
  c("region", "year", "item", "item_label"),
  comparison_table %>% filter(comparison == "protection_with_cdr")
)
write_csv(world_time,
          file.path(world_root, paste0("world_crp_area_yield_decomposition_over_time_to_", target_year, ".csv")))
world_time_long <- make_component_long(world_time)
p <- ggplot(world_time_long, aes(year, contribution_pp, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .35) +
  geom_col(width = 6.5) +
  geom_point(
    data = world_time,
    aes(year, production_change_pct, shape = "Total production change"),
    inherit.aes = FALSE, size = 2.7, colour = "black"
  ) +
  scale_fill_manual(values = component_colours, name = NULL) +
  scale_shape_manual(values = c("Total production change" = 18), name = NULL) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(
    title = "The area and yield channels of the protection effect change over time",
    subtitle = "Global AgMIP CRP response to adding protection under both CDR options (111 minus 011)",
    x = NULL, y = "Contribution to production change (percentage points)",
    caption = "Decadal AgMIP outputs. At each year, the two symmetric contributions close exactly to the black production-change marker."
  ) + decomp_theme + theme(panel.grid.major.x = element_blank())
ggsave(file.path(world_root, paste0("world_crp_area_yield_decomposition_over_time_to_", target_year, ".png")),
       p, width = 11.2, height = 6.5, dpi = 300, bg = "white")

# Validation summary --------------------------------------------------------

validation <- bind_rows(
  world_decomposition %>% transmute(level = "world", closure_pj),
  r10_decomposition %>% transmute(level = "r10", closure_pj),
  country_decomposition %>% transmute(level = "country", closure_pj),
  world_time %>% transmute(level = "world_time", closure_pj)
) %>%
  group_by(level) %>%
  summarise(
    n = n(), max_abs_closure_pj = max(abs(closure_pj), na.rm = TRUE),
    .groups = "drop"
  )
write_csv(validation,
          file.path(analysis_root, paste0("area_yield_decomposition_validation_", target_year, ".csv")))

message("Wrote AgMIP area-yield decomposition outputs to ", analysis_root)
