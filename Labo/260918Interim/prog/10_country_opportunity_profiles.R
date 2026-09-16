suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(ggrepel)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

analysis_root <- file.path(output_dir, "10_country_opportunity_profiles")
unlink(analysis_root, recursive = TRUE)
interaction_root <- file.path(analysis_root, "01_main_effect_and_interaction")
tradeoff_root <- file.path(analysis_root, "02_environment_agriculture_tradeoffs")
land_market_root <- file.path(analysis_root, "03_land_market_link")
character_root <- file.path(analysis_root, "04_country_characteristics")
for (path in c(interaction_root, tradeoff_root, land_market_root, character_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

story_theme <- theme_paper() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 7)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 10)),
    plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"), legend.position = "bottom"
  )

# AgMIP country effects and interaction terms -------------------------------

agri_file <- file.path(
  output_dir, "07_conditional_protection_decomposition", "01_agriculture",
  "country", paste0("country_conditional_protection_decomposition_", target_year, ".csv")
)
agri_raw <- read_csv(agri_file, show_col_types = FALSE) %>%
  filter(item == "CRP", indicator %in% c(
    "Area", "Production", "Consumption", "Producer price",
    "Food intake", "Imports", "Exports"
  )) %>%
  mutate(
    indicator_key = recode(
      indicator, `Producer price` = "producer_price",
      `Food intake` = "food_intake",
      Area = "area", Production = "production", Consumption = "consumption",
      Imports = "imports", Exports = "exports"
    ),
    interaction_pct = protection_under_mitigation_pct - protection_main_pct,
    interaction_value = protection_under_mitigation - protection_main
  )

agri_wide <- agri_raw %>%
  select(
    country, indicator_key,
    baseline = `011`, total_value = protection_under_mitigation,
    total_pct = protection_under_mitigation_pct,
    main_pct = protection_main_pct, interaction_pct,
    protection_x_afforestation_pct, protection_x_beccs_pct,
    protection_x_afforestation_x_beccs_pct
  ) %>%
  pivot_wider(
    names_from = indicator_key,
    values_from = c(
      baseline, total_value, total_pct, main_pct, interaction_pct,
      protection_x_afforestation_pct, protection_x_beccs_pct,
      protection_x_afforestation_x_beccs_pct
    ),
    names_glue = "{indicator_key}_{.value}"
  ) %>%
  mutate(
    crop_import_dependence_pct = if_else(
      consumption_baseline > 1e-10, 100 * imports_baseline / consumption_baseline,
      NA_real_
    ),
    crop_trade_openness_pct = if_else(
      consumption_baseline > 1e-10,
      100 * (imports_baseline + exports_baseline) / consumption_baseline,
      NA_real_
    )
  )

# Population, GDP and exact country-to-R10 membership from the AgMIP GDX.
agmip_file <- file.path(data_dir, "ALPHA", "AGMIP_all_scenario.gdx")
countries <- as.character(rgdx.set(agmip_file, "cty")[[1]])
request <- list(
  name = "AGMIP", form = "full",
  uels = list(
    unname(scenario_states("global2")["011"]), countries,
    c("POPT", "GDPT"), "TOT", c("million", "bn USD 2017 PPP"),
    as.character(target_year)
  )
)
socio_array <- rgdx(agmip_file, request, squeeze = FALSE, useDomInfo = TRUE)
socio <- as.data.frame.table(
  socio_array$val, responseName = "value", stringsAsFactors = FALSE
)
names(socio) <- c("scenario", "country", "variable", "item", "unit", "year", "value")
socio <- socio %>%
  filter(value != 0) %>%
  select(country, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  transmute(
    country, population_million = POPT,
    gdp_reported_gdx_value = GDPT,
    gdp_per_capita_2017_usd_ppp = GDPT / POPT
  )
country_r10 <- rgdx.set(agmip_file, "cty_reg_map") %>%
  transmute(country = as.character(cty), r10 = as.character(reg)) %>%
  filter(grepl("^R10", r10))

# Land redistribution and net land responses -------------------------------

grid_country <- read_csv(
  file.path(output_dir, "09_land_dynamics_story", "02_protection_pushout",
            paste0("country_grid_policy_dynamics_", target_year, ".csv")),
  show_col_types = FALSE
) %>%
  filter(comparison == "protection_with_both_cdr") %>%
  select(country, land_class, gain, loss, net_change_mha, relocated_mha) %>%
  mutate(class_key = recode(
    land_class, `Food cropland` = "food_grid",
    `2G bioenergy cropland` = "bio_grid", `Planted forest` = "forest_grid"
  )) %>%
  select(-land_class) %>%
  pivot_wider(
    names_from = class_key,
    values_from = c(gain, loss, net_change_mha, relocated_mha),
    names_glue = "{class_key}_{.value}"
  )

land_country <- read_csv(
  file.path(output_dir, "08_land_use_protection", "02_protection_displacement",
            "protection_land_displacement_all_levels.csv"),
  show_col_types = FALSE
) %>%
  filter(
    geography_level == "country",
    land_class %in% c("Food cropland", "2G bioenergy cropland", "Afforestation")
  ) %>%
  transmute(
    country = geography,
    class_key = recode(
      land_class, `Food cropland` = "food_merged",
      `2G bioenergy cropland` = "bio_merged", Afforestation = "aff_merged"
    ),
    net_mha = protection_under_mitigation_mha,
    net_pct = protection_under_mitigation_pct
  ) %>%
  pivot_wider(
    names_from = class_key, values_from = c(net_mha, net_pct),
    names_glue = "{class_key}_{.value}"
  )

luc <- read_csv(
  file.path(output_dir, "06_protection_focus", "04_climate_biodiversity",
            paste0("country_luc_emission_protection_", target_year, ".csv")),
  show_col_types = FALSE
) %>%
  transmute(
    country,
    luc_reduction_without_mitigation_mtco2 = `Without mitigation`,
    luc_reduction_under_mitigation_mtco2 = `Under mitigation`,
    luc_reduction_interaction_mtco2 = Interaction
  )

# Area-weighted country BII response, using the same 111-011 comparison.
policy_states <- scenario_states("global2")
grid_years <- c(2005, 2010, seq(2020, 2100, 10))
year_position <- match(target_year, grid_years)
read_bii <- function(state) {
  rast(
    file.path(data_dir, "BII", "nc",
              paste0("BIImap_", policy_states[[state]], "_default.nc")),
    subds = "BII"
  )[[year_position]]
}
land_template <- rast(
  file.path(data_dir, "PLUM", "nc",
            paste0("AIM-LUmap_", policy_states[["011"]], ".nc")),
  subds = "pixel_area"
)
bii_change <- resample((read_bii("111") - read_bii("011")) * 100,
                       land_template, method = "bilinear")
names(bii_change) <- "bii_change_points"
cell_area <- cellSize(bii_change, unit = "km")
valid_area <- ifel(!is.na(bii_change), cell_area, NA)
weighted_bii <- bii_change * cell_area
names(weighted_bii) <- "weighted_bii"
names(valid_area) <- "valid_area_km2"

world_sf <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(iso_a3 != "-99") %>%
  mutate(zone_id = row_number()) %>%
  select(zone_id, country = iso_a3)
zones <- rasterize(vect(world_sf), land_template, field = "zone_id")
bii_zonal <- zonal(c(weighted_bii, valid_area), zones, fun = "sum", na.rm = TRUE) %>%
  as_tibble()
names(bii_zonal)[1] <- "zone_id"
bii_country <- bii_zonal %>%
  left_join(st_drop_geometry(world_sf), by = "zone_id") %>%
  transmute(
    country,
    bii_change_points = weighted_bii / valid_area_km2,
    bii_valid_area_million_km2 = valid_area_km2 / 1e6
  )

country_profile <- agri_wide %>%
  left_join(socio, by = "country") %>%
  left_join(country_r10, by = "country") %>%
  left_join(grid_country, by = "country") %>%
  left_join(land_country, by = "country") %>%
  left_join(luc, by = "country") %>%
  left_join(bii_country, by = "country")
write_csv(country_profile,
          file.path(analysis_root, paste0("country_integrated_profile_", target_year, ".csv")))

# Continuous country scores and descriptive associations -------------------

# Each component is a within-sample percentile. The equal-weight composite is
# only an exploration aid; it does not assign countries to fixed classes.
country_scores <- country_profile %>%
  mutate(
    biodiversity_benefit_percentile = percent_rank(bii_change_points),
    production_favourability_percentile = percent_rank(production_total_pct),
    intake_favourability_percentile = percent_rank(food_intake_total_pct),
    price_favourability_percentile = percent_rank(-producer_price_total_pct),
    agricultural_favourability_score = rowMeans(cbind(
      production_favourability_percentile,
      intake_favourability_percentile,
      price_favourability_percentile
    )),
    biodiversity_agriculture_score = rowMeans(cbind(
      biodiversity_benefit_percentile, agricultural_favourability_score
    )),
    interaction_magnitude_score = rowMeans(cbind(
      percent_rank(abs(production_interaction_pct)),
      percent_rank(abs(producer_price_interaction_pct)),
      percent_rank(abs(food_intake_interaction_pct))
    ))
  ) %>%
  select(
    country, r10, population_million, gdp_per_capita_2017_usd_ppp,
    crop_import_dependence_pct, crop_trade_openness_pct,
    bii_change_points, biodiversity_benefit_percentile,
    production_total_pct, producer_price_total_pct, food_intake_total_pct,
    agricultural_favourability_score, biodiversity_agriculture_score,
    production_interaction_pct, producer_price_interaction_pct,
    food_intake_interaction_pct, interaction_magnitude_score
  ) %>%
  arrange(desc(biodiversity_agriculture_score))
write_csv(
  country_scores,
  file.path(analysis_root, paste0("country_continuous_scores_", target_year, ".csv"))
)

association_specs <- tribble(
  ~x_key, ~x_metric, ~y_key, ~y_metric,
  "food_grid_relocated_mha", "Food-cropland relocation", "production_total_pct", "Crop production",
  "food_grid_relocated_mha", "Food-cropland relocation", "producer_price_total_pct", "Producer price",
  "food_grid_relocated_mha", "Food-cropland relocation", "food_intake_total_pct", "Food intake",
  "crop_import_dependence_pct", "Baseline crop import dependence", "production_total_pct", "Crop production",
  "crop_import_dependence_pct", "Baseline crop import dependence", "producer_price_total_pct", "Producer price",
  "crop_import_dependence_pct", "Baseline crop import dependence", "food_intake_total_pct", "Food intake",
  "crop_trade_openness_pct", "Baseline crop trade openness", "production_total_pct", "Crop production",
  "crop_trade_openness_pct", "Baseline crop trade openness", "producer_price_total_pct", "Producer price",
  "crop_trade_openness_pct", "Baseline crop trade openness", "food_intake_total_pct", "Food intake",
  "gdp_per_capita_2017_usd_ppp", "GDP per capita", "production_total_pct", "Crop production",
  "gdp_per_capita_2017_usd_ppp", "GDP per capita", "producer_price_total_pct", "Producer price",
  "gdp_per_capita_2017_usd_ppp", "GDP per capita", "food_intake_total_pct", "Food intake",
  "bii_change_points", "BII response", "production_total_pct", "Crop production",
  "bii_change_points", "BII response", "producer_price_total_pct", "Producer price",
  "bii_change_points", "BII response", "food_intake_total_pct", "Food intake"
)
country_associations <- bind_rows(lapply(seq_len(nrow(association_specs)), function(i) {
  spec <- association_specs[i, ]
  pair <- country_profile %>%
    transmute(x = .data[[spec$x_key]], y = .data[[spec$y_key]]) %>%
    filter(is.finite(x), is.finite(y))
  tibble(
    x_key = spec$x_key, x_metric = spec$x_metric,
    y_key = spec$y_key, y_metric = spec$y_metric,
    n = nrow(pair),
    spearman_rho = if (nrow(pair) >= 3) cor(pair$x, pair$y, method = "spearman") else NA_real_,
    pearson_r = if (nrow(pair) >= 3) cor(pair$x, pair$y, method = "pearson") else NA_real_
  )
}))
write_csv(
  country_associations,
  file.path(character_root, paste0("country_association_summary_", target_year, ".csv"))
)

# Main protection effect versus interaction --------------------------------

interaction_plot_data_all <- agri_raw %>%
  filter(indicator %in% c("Production", "Producer price", "Food intake")) %>%
  left_join(country_profile %>% select(country, population_million, bii_change_points),
            by = "country") %>%
  group_by(indicator) %>%
  mutate(
    display = abs(protection_main_pct) <= quantile(abs(protection_main_pct), .99, na.rm = TRUE) &
      abs(interaction_pct) <= quantile(abs(interaction_pct), .99, na.rm = TRUE),
    label_score = rank(-(abs(protection_main_pct) + abs(interaction_pct)),
                       ties.method = "first"),
    label = if_else(label_score <= 7, country, NA_character_)
  ) %>%
  ungroup()
interaction_plot_data <- interaction_plot_data_all %>% filter(display)
colour_limit <- quantile(abs(interaction_plot_data$protection_under_mitigation_pct),
                         .98, na.rm = TRUE)
p <- ggplot(interaction_plot_data,
            aes(protection_main_pct, interaction_pct,
                size = population_million,
                colour = protection_under_mitigation_pct)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_abline(slope = -1, intercept = 0, colour = "grey45", linetype = 3) +
  geom_point(alpha = 0.72) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(indicator), scales = "free", ncol = 3) +
  scale_colour_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                         midpoint = 0, limits = c(-colour_limit, colour_limit),
                         oob = squish, name = "Total 111−011\n(% of baseline)") +
  scale_size_area(max_size = 10, name = "Population\n(million)") +
  labs(
    title = "Which countries have strong protection × CDR interactions?",
    subtitle = "Vertical position shows how afforestation and BECCS change the protection effect",
    x = "Protection main effect, 100−000 (% of mitigation baseline)",
    y = "Interaction: (111−011) − (100−000) (% of mitigation baseline)",
    caption = "The dotted diagonal marks zero total effect. Positive is an increase in the named indicator; desirability therefore differs for production, price and intake."
  ) + story_theme
ggsave(file.path(interaction_root, paste0("country_main_effect_vs_interaction_", target_year, ".png")),
       p, width = 14, height = 6.2, dpi = 300, bg = "white")
write_csv(interaction_plot_data_all,
          file.path(interaction_root, paste0("country_main_effect_vs_interaction_", target_year, ".csv")))

# Dumbbells show directly how the CDR context moves the protection effect.
dumbbell_data <- interaction_plot_data_all %>%
  group_by(indicator) %>%
  slice_max(pmax(abs(protection_main_pct), abs(protection_under_mitigation_pct)),
            n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(country_panel = interaction(country, indicator, lex.order = TRUE))
p <- ggplot(dumbbell_data, aes(y = reorder(country_panel, protection_under_mitigation_pct))) +
  geom_segment(aes(x = protection_main_pct, xend = protection_under_mitigation_pct,
                   yend = reorder(country_panel, protection_under_mitigation_pct)),
               colour = "grey60", linewidth = 0.8) +
  geom_point(aes(x = protection_main_pct, colour = "Without CDR"), size = 2.5) +
  geom_point(aes(x = protection_under_mitigation_pct, colour = "With both CDR"), size = 2.5) +
  facet_wrap(vars(indicator), scales = "free", ncol = 3) +
  scale_y_discrete(labels = function(x) sub("[.]", "", sub("[.].*$", "", x))) +
  scale_colour_manual(values = c("Without CDR" = "#756bb1", "With both CDR" = "#238b45"),
                      name = "Protection effect") +
  labs(
    title = "How the CDR context changes country-level protection effects",
    subtitle = "Countries shown have the largest main or mitigation-context effects in each indicator",
    x = "Protection-induced change (% of mitigation baseline)", y = NULL,
    caption = "The connecting line is the interaction term. Selection is only for display; all countries remain in the CSV."
  ) + story_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(interaction_root, paste0("country_protection_context_dumbbells_", target_year, ".png")),
       p, width = 14, height = 9, dpi = 300, bg = "white")

# Environmental benefit versus agricultural outcomes -----------------------

tradeoff_long <- country_profile %>%
  select(country, population_million, r10, bii_change_points,
         luc_reduction_under_mitigation_mtco2,
         production_total_pct, food_intake_total_pct, producer_price_total_pct) %>%
  pivot_longer(
    c(production_total_pct, food_intake_total_pct, producer_price_total_pct),
    names_to = "outcome", values_to = "raw_outcome_pct"
  ) %>%
  mutate(
    outcome_label = recode(
      outcome,
      production_total_pct = "Crop production change",
      food_intake_total_pct = "Food intake change",
      producer_price_total_pct = "Less producer-price pressure"
    ),
    agricultural_outcome_pct = if_else(
      outcome == "producer_price_total_pct", -raw_outcome_pct, raw_outcome_pct
    )
  ) %>%
  filter(is.finite(agricultural_outcome_pct), is.finite(bii_change_points)) %>%
  group_by(outcome_label) %>%
  mutate(
    combined_rank = percent_rank(agricultural_outcome_pct) + percent_rank(bii_change_points),
    label = if_else(rank(-combined_rank, ties.method = "first") <= 6,
                    country, NA_character_),
    display = abs(agricultural_outcome_pct) <=
      quantile(abs(agricultural_outcome_pct), .99, na.rm = TRUE) &
      abs(bii_change_points) <= quantile(abs(bii_change_points), .99, na.rm = TRUE)
  ) %>% ungroup() %>% filter(display)
emission_limit <- quantile(abs(tradeoff_long$luc_reduction_under_mitigation_mtco2),
                           .98, na.rm = TRUE)
p <- ggplot(tradeoff_long,
            aes(agricultural_outcome_pct, bii_change_points,
                size = population_million,
                colour = luc_reduction_under_mitigation_mtco2)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(alpha = 0.72) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(outcome_label), scales = "free_x", ncol = 3) +
  scale_colour_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#238b45",
                         midpoint = 0, limits = c(-emission_limit, emission_limit),
                         oob = squish, name = "LUC emission\nreduction\n(Mt CO2/yr)") +
  scale_size_area(max_size = 10, name = "Population\n(million)") +
  labs(
    title = "Countries differ in biodiversity benefit and agricultural trade-off",
    subtitle = "Upper-right points combine a BII gain with a more favourable agricultural response",
    x = "Agricultural outcome under protection (% of mitigation baseline; right is more favourable)",
    y = "Area-weighted BII change after protection (points)",
    caption = "Producer-price change is sign-reversed so that right always means less agricultural pressure. Labels mark high combined continuous ranks, not a fixed class."
  ) + story_theme
ggsave(file.path(tradeoff_root, paste0("country_bii_agriculture_bubbles_", target_year, ".png")),
       p, width = 14, height = 6.2, dpi = 300, bg = "white")
write_csv(tradeoff_long,
          file.path(tradeoff_root, paste0("country_bii_agriculture_bubbles_", target_year, ".csv")))

# Land movement versus market response -------------------------------------

land_market <- country_profile %>%
  select(country, population_million, food_grid_relocated_mha,
         food_grid_net_change_mha, production_total_pct,
         food_intake_total_pct, producer_price_total_pct) %>%
  pivot_longer(
    c(production_total_pct, food_intake_total_pct, producer_price_total_pct),
    names_to = "market_metric", values_to = "market_change_pct"
  ) %>%
  mutate(market_metric = recode(
    market_metric,
    production_total_pct = "Crop production",
    food_intake_total_pct = "Food intake",
    producer_price_total_pct = "Producer price"
  )) %>%
  filter(is.finite(food_grid_relocated_mha), is.finite(market_change_pct)) %>%
  group_by(market_metric) %>%
  mutate(
    label = if_else(rank(-food_grid_relocated_mha, ties.method = "first") <= 6,
                    country, NA_character_),
    display = abs(market_change_pct) <= quantile(abs(market_change_pct), .99, na.rm = TRUE)
  ) %>% ungroup() %>% filter(display)
net_limit <- quantile(abs(land_market$food_grid_net_change_mha), .98, na.rm = TRUE)
p <- ggplot(land_market,
            aes(food_grid_relocated_mha, market_change_pct,
                size = population_million, colour = food_grid_net_change_mha)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(alpha = 0.72) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(market_metric), scales = "free_y", ncol = 3) +
  scale_x_sqrt() +
  scale_colour_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                         midpoint = 0, limits = c(-net_limit, net_limit), oob = squish,
                         name = "Net food-land\nchange (Mha)") +
  scale_size_area(max_size = 10, name = "Population\n(million)") +
  labs(
    title = "Large food-cropland relocation does not always create a large market effect",
    subtitle = "Horizontal position measures gross cell relocation after protection; vertical position shows the AgMIP response",
    x = "Food-cropland area relocated within country (million ha, square-root scale)",
    y = "Protection-induced market change (% of mitigation baseline)",
    caption = "Land movement is an exploratory AIM-LU grid measure; market outcomes are AgMIP CRP results."
  ) + story_theme
ggsave(file.path(land_market_root, paste0("country_land_relocation_vs_market_", target_year, ".png")),
       p, width = 14, height = 6.2, dpi = 300, bg = "white")
write_csv(land_market,
          file.path(land_market_root, paste0("country_land_relocation_vs_market_", target_year, ".csv")))

# Country characteristics associated with market response ------------------

character_data <- country_profile %>%
  select(country, population_million, gdp_per_capita_2017_usd_ppp,
         crop_import_dependence_pct, crop_trade_openness_pct,
         production_total_pct, producer_price_total_pct, food_intake_total_pct,
         bii_change_points) %>%
  pivot_longer(
    c(production_total_pct, producer_price_total_pct, food_intake_total_pct),
    names_to = "outcome", values_to = "change_pct"
  ) %>%
  mutate(outcome = recode(
    outcome, production_total_pct = "Crop production",
    producer_price_total_pct = "Producer price", food_intake_total_pct = "Food intake"
  )) %>%
  filter(gdp_per_capita_2017_usd_ppp > 0,
         is.finite(crop_import_dependence_pct), is.finite(change_pct)) %>%
  group_by(outcome) %>%
  mutate(
    label_score = abs(change_pct) + abs(change_pct) /
      max(abs(change_pct), na.rm = TRUE),
    label = if_else(rank(-label_score, ties.method = "first") <= 6,
                    country, NA_character_),
    display = crop_import_dependence_pct <=
      quantile(crop_import_dependence_pct, .99, na.rm = TRUE) &
      abs(change_pct) <= quantile(abs(change_pct), .99, na.rm = TRUE)
  ) %>% ungroup() %>% filter(display)
bii_limit <- quantile(abs(character_data$bii_change_points), .98, na.rm = TRUE)
p <- ggplot(character_data,
            aes(crop_import_dependence_pct, change_pct,
                size = population_million, colour = bii_change_points)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(alpha = 0.72) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 3) +
  scale_colour_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                         midpoint = 0, limits = c(-bii_limit, bii_limit), oob = squish,
                         name = "BII change\n(points)") +
  scale_size_area(max_size = 10, name = "Population\n(million)") +
  labs(
    title = "Does baseline import dependence buffer country-level impacts?",
    subtitle = "Countries with similar land pressure can differ because production and trade adjust together",
    x = "Baseline crop import dependence: imports / consumption (%)",
    y = "Protection-induced change (% of mitigation baseline)",
    caption = "Import dependence is calculated from AgMIP CRP energy quantities in scenario 011. Association is descriptive, not causal."
  ) + story_theme
ggsave(file.path(character_root, paste0("country_import_dependence_vs_impacts_", target_year, ".png")),
       p, width = 14, height = 6.2, dpi = 300, bg = "white")

p <- ggplot(character_data,
            aes(gdp_per_capita_2017_usd_ppp, change_pct,
                size = population_million, colour = crop_import_dependence_pct)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_point(alpha = 0.72) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 3) +
  scale_x_log10(labels = label_number()) +
  scale_colour_viridis_c(option = "C", trans = "sqrt",
                         name = "Import dependence\n(%)") +
  scale_size_area(max_size = 10, name = "Population\n(million)") +
  labs(
    title = "Country income alone does not determine the agricultural response",
    subtitle = "Bubble colour adds baseline crop import dependence to the income–impact relationship",
    x = "GDP per capita (2017 USD PPP/person, log scale)",
    y = "Protection-induced change (% of mitigation baseline)",
    caption = "GDP, population and import dependence use the mitigation baseline scenario 011."
  ) + story_theme
ggsave(file.path(character_root, paste0("country_income_trade_vs_impacts_", target_year, ".png")),
       p, width = 14, height = 6.2, dpi = 300, bg = "white")
write_csv(character_data,
          file.path(character_root, paste0("country_characteristics_vs_impacts_", target_year, ".csv")))

association_heatmap <- country_associations %>%
  mutate(
    x_metric = factor(x_metric, levels = rev(c(
      "Food-cropland relocation", "Baseline crop import dependence",
      "Baseline crop trade openness", "GDP per capita", "BII response"
    ))),
    y_metric = factor(y_metric, levels = c("Crop production", "Producer price", "Food intake")),
    cell_label = sprintf("%+.2f\n(n=%d)", spearman_rho, n)
  )
p <- ggplot(association_heatmap, aes(y_metric, x_metric, fill = spearman_rho)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = cell_label), size = 3.4) +
  scale_fill_gradient2(
    low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
    limits = c(-1, 1), name = "Spearman\nrho"
  ) +
  labs(
    title = "Country characteristics have tendencies, not deterministic types",
    subtitle = "Rank correlations summarise continuous cross-country associations",
    x = "Protection-induced AgMIP CRP response (111−011)", y = NULL,
    caption = "Positive means that a larger row variable is associated with a larger column response. Associations are descriptive and do not identify causal mechanisms."
  ) + story_theme +
  theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold"))
ggsave(
  file.path(character_root, paste0("country_association_heatmap_", target_year, ".png")),
  p, width = 10.5, height = 5.6, dpi = 300, bg = "white"
)

rm(bii_change, weighted_bii, valid_area, zones)
gc()
