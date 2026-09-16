suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

analysis_root <- file.path(output_dir, "09_land_dynamics_story")
unlink(analysis_root, recursive = TRUE)
matrix_root <- file.path(analysis_root, "01_policy_response_matrix")
pushout_root <- file.path(analysis_root, "02_protection_pushout")
food_root <- file.path(analysis_root, "03_cdr_food_redistribution")
evidence_root <- file.path(analysis_root, "04_overlap_evidence")
for (path in c(matrix_root, pushout_root, food_root, evidence_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

states <- scenario_states("global2")
r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)

# Each row is a marginal policy experiment. The final row is the main
# protection comparison used throughout the protection-focused analysis.
contrasts <- tribble(
  ~comparison, ~from, ~to, ~comparison_label, ~short_label,
  "protection_no_cdr", "000", "100", "Protection added\nwithout CDR", "Protection | no CDR",
  "afforestation_no_protection", "000", "010", "Afforestation added\nwithout protection", "Afforestation",
  "beccs_no_protection", "000", "001", "BECCS added\nwithout protection", "BECCS",
  "both_cdr_no_protection", "000", "011", "Both CDR options added\nwithout protection", "Both CDR",
  "protection_with_afforestation", "010", "110", "Protection added\nwith afforestation", "Protection | afforestation",
  "protection_with_beccs", "001", "101", "Protection added\nwith BECCS", "Protection | BECCS",
  "protection_with_both_cdr", "011", "111", "Protection added\nwith both CDR options", "Protection | both CDR"
)
contrast_levels <- contrasts$comparison_label

story_theme <- theme_paper() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 7)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 10)),
    plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold")
  )

# MergedComparison: reliable aggregate accounting --------------------------

merged_variables <- c(
  "Lan_Cov", "Lan_Cov_Bui_Are", "Lan_Cov_Cro",
  "Lan_Cov_Cro_Non_Ene_Cro", "Lan_Cov_Cro_Ene_Cro_2nd_gen",
  "Lan_Cov_Frs", "Lan_Cov_Frs_Aff", "Lan_Cov_Frs_Ref",
  "Lan_Cov_Oth_Lan", "Lan_Cov_Oth_Nat_Lan", "Lan_Cov_Pst"
)
merged_raw <- rgdx.param(
  file.path(data_dir, "PLUM", "Mergedcomparison.gdx"), "IAMCTempwoU"
) %>%
  filter(
    i1 %in% unname(states),
    i2 %in% c("WLD", names(r10_labels)),
    i3 %in% merged_variables,
    as.integer(as.character(i4)) == target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    geography = as.character(i2), variable = as.character(i3),
    value = as.numeric(value)
  ) %>%
  group_by(state, geography, variable) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

merged_wide <- merged_raw %>%
  complete(state = names(states), geography = c("WLD", names(r10_labels)),
           variable = merged_variables, fill = list(value = 0)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    `Food cropland` = Lan_Cov_Cro_Non_Ene_Cro,
    `2G bioenergy cropland` = Lan_Cov_Cro_Ene_Cro_2nd_gen,
    `Fallow cropland` = Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro -
      Lan_Cov_Cro_Ene_Cro_2nd_gen,
    Forest = Lan_Cov_Frs, Afforestation = Lan_Cov_Frs_Aff,
    Reforestation = Lan_Cov_Frs_Ref, Pasture = Lan_Cov_Pst,
    `Other natural land` = Lan_Cov_Oth_Nat_Lan,
    `Built-up area` = Lan_Cov_Bui_Are,
    `Other land outside reported total` = Lan_Cov_Oth_Lan
  )

story_classes <- c(
  "Food cropland", "2G bioenergy cropland", "Afforestation",
  "Forest", "Pasture", "Other natural land", "Fallow cropland"
)
merged_state_wide <- merged_wide %>%
  select(geography, state, all_of(story_classes)) %>%
  pivot_longer(all_of(story_classes), names_to = "land_class",
               values_to = "area_mha") %>%
  pivot_wider(names_from = state, values_from = area_mha)

policy_response <- lapply(seq_len(nrow(contrasts)), function(k) {
  from_state <- contrasts$from[[k]]
  to_state <- contrasts$to[[k]]
  merged_state_wide %>%
    transmute(
      geography, land_class,
      comparison = contrasts$comparison[[k]],
      comparison_label = contrasts$comparison_label[[k]],
      from_state, to_state,
      baseline_mha = .data[[from_state]],
      policy_mha = .data[[to_state]],
      net_change_mha = policy_mha - baseline_mha,
      change_pct_of_baseline = if_else(
        abs(baseline_mha) > 1e-10, 100 * net_change_mha / baseline_mha, NA_real_
      )
    )
}) %>% bind_rows() %>%
  mutate(
    comparison_label = factor(comparison_label, levels = contrast_levels),
    land_class = factor(land_class, levels = rev(story_classes))
  )
write_csv(policy_response,
          file.path(matrix_root, paste0("merged_policy_response_all_levels_", target_year, ".csv")))

world_matrix <- policy_response %>% filter(geography == "WLD")
matrix_limit <- quantile(abs(world_matrix$net_change_mha), 0.95, na.rm = TRUE)
p <- ggplot(world_matrix,
            aes(comparison_label, land_class, fill = net_change_mha)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%+.1f\nMha", net_change_mha)), size = 3) +
  scale_fill_gradient2(
    low = "#b2182b", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
    limits = c(-matrix_limit, matrix_limit), oob = squish,
    name = "Net change\n(million ha)"
  ) +
  labs(
    title = "How each land policy changes global land allocation",
    subtitle = "Cell labels and colour show net area change from the starting state of each comparison",
    x = NULL, y = NULL,
    caption = "Source: AIM-PLUM MergedComparison. Afforestation is a subset of Forest and is shown as a diagnostic row."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom")
ggsave(file.path(matrix_root, paste0("world_policy_response_matrix_", target_year, ".png")),
       p, width = 13.5, height = 7.7, dpi = 300, bg = "white")

# A sequential accounting path: first add both CDR options, then protection.
world_path <- merged_state_wide %>%
  filter(geography == "WLD") %>%
  transmute(
    land_class,
    cdr_effect = `011` - `000`,
    protection_after_cdr = `111` - `011`,
    total_effect = `111` - `000`
  ) %>%
  pivot_longer(-land_class, names_to = "step", values_to = "change_mha") %>%
  mutate(
    step = factor(recode(
      step,
      cdr_effect = "1. Add afforestation + BECCS",
      protection_after_cdr = "2. Add protection",
      total_effect = "Final change from BaU"
    ), levels = c("1. Add afforestation + BECCS", "2. Add protection",
                  "Final change from BaU")),
    land_class = factor(land_class, levels = rev(story_classes))
  )
p <- ggplot(world_path, aes(change_mha, land_class, fill = step)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.35) +
  geom_col(position = position_dodge(width = 0.76), width = 0.7) +
  scale_fill_manual(values = c("#756bb1", "#238b45", "#252525"), name = NULL) +
  labs(
    title = "One policy path from BaU to the combined scenario",
    subtitle = "The green bar is the additional land response when protection is introduced after both CDR options",
    x = "Net area change (million ha)", y = NULL,
    caption = "The first two bars add exactly to the final change. They describe a scenario path, not physical parcel-by-parcel flows."
  ) + story_theme + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path(matrix_root, paste0("world_policy_path_cdr_then_protection_", target_year, ".png")),
       p, width = 11, height = 7.2, dpi = 300, bg = "white")
write_csv(world_path,
          file.path(matrix_root, paste0("world_policy_path_cdr_then_protection_", target_year, ".csv")))

r10_matrix <- policy_response %>%
  filter(geography %in% names(r10_labels),
         land_class %in% c("Food cropland", "2G bioenergy cropland", "Afforestation")) %>%
  mutate(region = factor(unname(r10_labels[geography]), levels = unname(r10_labels)))
r10_limit <- quantile(abs(r10_matrix$net_change_mha), 0.98, na.rm = TRUE)
p <- ggplot(r10_matrix,
            aes(comparison_label, land_class, fill = net_change_mha)) +
  geom_tile(colour = "white", linewidth = 0.35) +
  facet_wrap(vars(region), ncol = 2) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                       midpoint = 0, limits = c(-r10_limit, r10_limit),
                       oob = squish, name = "Net change\n(million ha)") +
  labs(
    title = "Regional land responses depend on which policy is already present",
    subtitle = "Food cropland, 2G bioenergy cropland and afforestation; red decreases and blue increases",
    x = NULL, y = NULL,
    caption = "Source: AIM-PLUM MergedComparison. Values use model-reported R10 aggregates."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7), legend.position = "bottom")
ggsave(file.path(matrix_root, paste0("r10_policy_response_matrix_", target_year, ".png")),
       p, width = 13, height = 15, dpi = 300, bg = "white")

# AIM-LU grid: gross losses, gains and relocation ---------------------------

grid_years <- c(2005, 2010, seq(2020, 2100, 10))
year_position <- match(target_year, grid_years)
grid_classes <- c(
  food = "Food cropland", bio = "2G bioenergy cropland",
  planted_forest = "Planted forest"
)
grid_class_index <- c(food = 1L, bio = 2L, planted_forest = 5L)

# TEMPORARY WORKAROUND: existing AIM-LUmap files double-count Rbasin and R17.
# Remove this factor after regenerating them with the corrected combine.gms.
grid_export_duplicate_factor <- 2

read_grid <- function(state) {
  path <- file.path(data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[[state]], ".nc"))
  x <- rast(path, subds = "LC_area_share")
  positions <- (unname(grid_class_index) - 1L) * length(grid_years) + year_position
  out <- x[[positions]] / grid_export_duplicate_factor
  names(out) <- names(grid_class_index)
  out
}
grid_by_state <- lapply(names(states), read_grid)
names(grid_by_state) <- names(states)
pixel_area <- rast(
  file.path(data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[["000"]], ".nc")),
  subds = "pixel_area"
)

# A country-zone raster supports gross gain/loss aggregation. Cells are assigned
# by their centre; results remain exploratory because the source grid itself has
# known out-of-range share values.
countries_sf <- ne_countries(scale = "small", returnclass = "sf") %>%
  filter(iso_a3 != "-99") %>%
  mutate(zone_id = row_number()) %>%
  select(zone_id, country = iso_a3)
country_zones <- rasterize(vect(countries_sf), pixel_area, field = "zone_id")

global_rows <- list()
area_layers <- NULL
layer_keys <- character()
row_id <- 1L
for (k in seq_len(nrow(contrasts))) {
  comp_id <- contrasts$comparison[[k]]
  from_state <- contrasts$from[[k]]
  to_state <- contrasts$to[[k]]
  for (class_id in names(grid_classes)) {
    delta_share <- grid_by_state[[to_state]][[class_id]] -
      grid_by_state[[from_state]][[class_id]]
    delta_area <- delta_share * pixel_area
    gain_area <- ifel(delta_area > 0, delta_area, 0)
    loss_area <- ifel(delta_area < 0, -delta_area, 0)
    gain <- global(gain_area, "sum", na.rm = TRUE)[[1]]
    loss <- global(loss_area, "sum", na.rm = TRUE)[[1]]
    baseline <- global(grid_by_state[[from_state]][[class_id]] * pixel_area,
                       "sum", na.rm = TRUE)[[1]]
    global_rows[[row_id]] <- tibble(
      comparison = comp_id,
      comparison_label = contrasts$comparison_label[[k]],
      from_state, to_state, class_id,
      land_class = unname(grid_classes[[class_id]]),
      baseline_mha = baseline, area_gained_mha = gain, area_lost_mha = loss,
      relocated_mha = min(gain, loss), net_change_mha = gain - loss,
      loss_offset_elsewhere_pct = if_else(loss > 1e-10,
                                          100 * min(gain, loss) / loss, NA_real_)
    )
    for (direction in c("gain", "loss")) {
      layer <- if (direction == "gain") gain_area else loss_area
      key <- paste(comp_id, class_id, direction, sep = "__")
      names(layer) <- key
      area_layers <- if (is.null(area_layers)) layer else c(area_layers, layer)
      layer_keys <- c(layer_keys, key)
    }
    row_id <- row_id + 1L
  }
}
global_grid_dynamics <- bind_rows(global_rows) %>%
  mutate(comparison_label = factor(comparison_label, levels = contrast_levels))
write_csv(global_grid_dynamics,
          file.path(pushout_root, paste0("world_grid_policy_dynamics_", target_year, ".csv")))

# Protection after both CDR options: show the push-out and compensation balance.
protection_balance <- global_grid_dynamics %>%
  filter(comparison == "protection_with_both_cdr")
balance_long <- protection_balance %>%
  select(land_class, area_gained_mha, area_lost_mha) %>%
  mutate(area_lost_mha = -area_lost_mha) %>%
  pivot_longer(-land_class, names_to = "direction", values_to = "area_mha") %>%
  mutate(direction = recode(direction,
                            area_gained_mha = "Gained in other cells",
                            area_lost_mha = "Lost from cells"))
p <- ggplot(balance_long, aes(area_mha, land_class, fill = direction)) +
  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.4) +
  geom_col(width = 0.62) +
  geom_point(data = protection_balance,
             aes(net_change_mha, land_class), inherit.aes = FALSE,
             shape = 18, size = 3.1, colour = "black") +
  geom_text(data = protection_balance,
            aes(x = 0, y = land_class,
                label = paste0("  net ", sprintf("%+.1f", net_change_mha), " Mha")),
            inherit.aes = FALSE, hjust = -0.02, vjust = -1.45, size = 3.2) +
  scale_fill_manual(values = c("Gained in other cells" = "#2166ac",
                               "Lost from cells" = "#b2182b"), name = NULL) +
  labs(
    title = "Where protection pushes land out, most area appears elsewhere",
    subtitle = "Protection is added after afforestation and BECCS: 111 minus 011. Diamonds show the small residual after gains and losses offset.",
    x = "Grid area affected (million ha; losses shown to the left)", y = NULL,
    caption = "AIM-LU 0.5° diagnostic. This measures gross cell redistribution, not direct overlap with the protection mask."
  ) + story_theme + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path(pushout_root, paste0("world_protection_pushout_balance_", target_year, ".png")),
       p, width = 11, height = 5.8, dpi = 300, bg = "white")

# Across all policy comparisons, show how much of the loss is offset elsewhere.
p <- global_grid_dynamics %>%
  ggplot(aes(comparison_label, land_class, fill = loss_offset_elsewhere_pct)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(aes(label = paste0(
    "loss ", sprintf("%.0f", area_lost_mha), "\n",
    "gain ", sprintf("%.0f", area_gained_mha), "\n",
    "net ", sprintf("%+.1f", net_change_mha)
  )), size = 2.8) +
  scale_fill_gradient(low = "#fff7ec", high = "#7f0000", limits = c(0, 100),
                      oob = squish, name = "Loss offset\nelsewhere (%)") +
  labs(
    title = "Gross redistribution reveals dynamics hidden by small net changes",
    subtitle = "Each tile reports lost area, gained area and their net balance in million ha",
    x = NULL, y = NULL,
    caption = "A value near 100% means almost all area lost from some cells is matched by gains in other cells."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "bottom")
ggsave(file.path(pushout_root, paste0("world_policy_redistribution_matrix_", target_year, ".png")),
       p, width = 13.5, height = 5.8, dpi = 300, bg = "white")

# Country and R10 gross gain/loss accounting from the same grid contrasts.
zonal_values <- zonal(area_layers, country_zones, fun = "sum", na.rm = TRUE) %>%
  as_tibble()
names(zonal_values)[1] <- "zone_id"
country_grid_long <- zonal_values %>%
  pivot_longer(-zone_id, names_to = "key", values_to = "area_mha") %>%
  separate(key, into = c("comparison", "class_id", "direction"), sep = "__") %>%
  left_join(st_drop_geometry(countries_sf), by = "zone_id") %>%
  filter(country %in% read_csv(file.path(data_dir, "country_mapping.csv"),
                               show_col_types = FALSE)$country) %>%
  pivot_wider(names_from = direction, values_from = area_mha, values_fill = 0) %>%
  mutate(
    land_class = unname(grid_classes[class_id]),
    net_change_mha = gain - loss,
    relocated_mha = pmin(gain, loss),
    loss_offset_elsewhere_pct = if_else(loss > 1e-10,
                                        100 * pmin(gain, loss) / loss, NA_real_)
  ) %>%
  left_join(contrasts %>% select(comparison, comparison_label), by = "comparison")
write_csv(country_grid_long,
          file.path(pushout_root, paste0("country_grid_policy_dynamics_", target_year, ".csv")))

# Use the exact country-to-R10 set shipped in the AgMIP GDX. This avoids
# ambiguous conversions from broader R5 groups such as OECD90.
country_r10 <- rgdx.set(
  file.path(data_dir, "ALPHA", "AGMIP_all_scenario.gdx"), "cty_reg_map"
) %>%
  transmute(country = as.character(cty), r10 = as.character(reg)) %>%
  filter(r10 %in% names(r10_labels))
r10_grid <- country_grid_long %>%
  left_join(country_r10 %>% select(country, r10), by = "country") %>%
  filter(!is.na(r10)) %>%
  group_by(r10, comparison, comparison_label, class_id, land_class) %>%
  summarise(gain = sum(gain, na.rm = TRUE), loss = sum(loss, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(net_change_mha = gain - loss, relocated_mha = pmin(gain, loss),
         region = unname(r10_labels[r10]))
write_csv(r10_grid,
          file.path(pushout_root, paste0("r10_grid_policy_dynamics_", target_year, ".csv")))

r10_protection <- r10_grid %>%
  filter(comparison == "protection_with_both_cdr") %>%
  select(region, land_class, gain, loss, net_change_mha) %>%
  mutate(loss = -loss) %>%
  pivot_longer(c(gain, loss), names_to = "direction", values_to = "area_mha") %>%
  mutate(direction = recode(direction, gain = "Gained cells", loss = "Lost cells"))
p <- ggplot(r10_protection, aes(area_mha, reorder(region, area_mha), fill = direction)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(width = 0.68) +
  facet_wrap(vars(land_class), scales = "free_x", ncol = 1) +
  scale_fill_manual(values = c("Gained cells" = "#2166ac", "Lost cells" = "#b2182b"),
                    name = NULL) +
  labs(
    title = "Protection redistributes CDR land and food cropland differently by region",
    subtitle = "Gross cell gains and losses after adding protection to the mitigation scenario",
    x = "Grid area affected (million ha)", y = NULL,
    caption = "R10 values are exploratory sums of country-centre grid assignments; use MergedComparison for final regional totals."
  ) + story_theme + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path(pushout_root, paste0("r10_protection_pushout_balance_", target_year, ".png")),
       p, width = 10, height = 12, dpi = 300, bg = "white")

# Food-cropland redistribution caused by CDR and protection -----------------

food_comparisons <- c("afforestation_no_protection", "beccs_no_protection",
                      "both_cdr_no_protection", "protection_with_both_cdr")
world_food <- global_grid_dynamics %>%
  filter(class_id == "food", comparison %in% food_comparisons) %>%
  select(comparison_label, area_gained_mha, area_lost_mha, net_change_mha) %>%
  mutate(area_lost_mha = -area_lost_mha) %>%
  pivot_longer(c(area_gained_mha, area_lost_mha),
               names_to = "direction", values_to = "area_mha") %>%
  mutate(direction = recode(direction,
                            area_gained_mha = "Food cropland gained",
                            area_lost_mha = "Food cropland lost"))
p <- ggplot(world_food, aes(area_mha, comparison_label, fill = direction)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.35) +
  geom_col(width = 0.62) +
  scale_fill_manual(values = c("Food cropland gained" = "#2166ac",
                               "Food cropland lost" = "#b2182b"), name = NULL) +
  labs(
    title = "CDR policies relocate far more food cropland than the global net change suggests",
    subtitle = "Separate marginal experiments show which policy context changes the spatial distribution of food cropland",
    x = "Food-cropland area affected (million ha)", y = NULL,
    caption = "Gross grid changes. These comparisons are counterfactual model contrasts and should not be added together."
  ) + story_theme + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path(food_root, paste0("world_food_cropland_redistribution_by_policy_", target_year, ".png")),
       p, width = 11.5, height = 6.3, dpi = 300, bg = "white")

r10_food <- r10_grid %>%
  filter(class_id == "food", comparison %in% food_comparisons) %>%
  mutate(loss = -loss, region = factor(region, levels = rev(unname(r10_labels)))) %>%
  pivot_longer(c(gain, loss), names_to = "direction", values_to = "area_mha") %>%
  mutate(direction = recode(direction, gain = "Food cropland gained",
                            loss = "Food cropland lost"))
p <- ggplot(r10_food, aes(region, area_mha, fill = direction)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(width = 0.68) + coord_flip() +
  facet_wrap(vars(comparison_label), scales = "free_x", ncol = 2) +
  scale_fill_manual(values = c("Food cropland gained" = "#2166ac",
                               "Food cropland lost" = "#b2182b"), name = NULL) +
  labs(
    title = "Where food cropland moves under afforestation, BECCS and protection",
    subtitle = "Regional gross gains and losses expose relocation that cancels in the world total",
    x = NULL, y = "Grid area affected (million ha)",
    caption = "Exploratory AIM-LU grid aggregation; panels are separate scenario contrasts."
  ) + story_theme + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path(food_root, paste0("r10_food_cropland_redistribution_by_policy_", target_year, ".png")),
       p, width = 13, height = 10, dpi = 300, bg = "white")

# Screening map: which separate marginal experiment produces the largest gross
# loss of food cropland in each country? The effects are not additive.
driver_labels <- c(
  afforestation_no_protection = "Afforestation",
  beccs_no_protection = "BECCS",
  protection_with_both_cdr = "Protection under both CDR"
)
food_driver <- country_grid_long %>%
  filter(class_id == "food", comparison %in% names(driver_labels), loss > 1e-10) %>%
  group_by(country) %>%
  slice_max(loss, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(dominant_driver = factor(unname(driver_labels[comparison]),
                                  levels = unname(driver_labels)))
mapped_driver <- countries_sf %>% inner_join(food_driver, by = "country")
p <- ggplot() +
  geom_sf(data = countries_sf, fill = "grey90", colour = "grey72", linewidth = 0.12) +
  geom_sf(data = mapped_driver, aes(fill = dominant_driver),
          colour = "grey72", linewidth = 0.12) +
  scale_fill_manual(values = c("Afforestation" = "#1b9e77", "BECCS" = "#d95f02",
                               "Protection under both CDR" = "#7570b3"),
                    name = "Largest gross loss") +
  coord_sf(expand = FALSE) +
  labs(
    title = "Which policy contrast produces the largest food-cropland loss?",
    subtitle = "Country screening based on gross losses from grid cells",
    caption = "Afforestation and BECCS are each compared with BaU; protection is compared with the both-CDR scenario. Contrasts are not additive."
  ) + theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10.5, colour = "grey30"),
        plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0),
        legend.position = "bottom")
ggsave(file.path(food_root, paste0("country_dominant_food_cropland_pressure_", target_year, ".png")),
       p, width = 11.5, height = 6.2, dpi = 300, bg = "white")
write_csv(food_driver,
          file.path(food_root, paste0("country_dominant_food_cropland_pressure_", target_year, ".csv")))

country_scatter <- country_grid_long %>%
  filter(comparison == "protection_with_both_cdr") %>%
  group_by(land_class) %>%
  mutate(label_country = if_else(rank(-(gain + loss), ties.method = "first") <= 6,
                                 country, NA_character_)) %>%
  ungroup()
max_axis <- quantile(c(country_scatter$gain, country_scatter$loss), 0.99, na.rm = TRUE)
country_scatter_display <- country_scatter %>%
  filter(gain <= max_axis, loss <= max_axis)
p <- ggplot(country_scatter_display, aes(loss, gain)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey45", linetype = 2) +
  geom_point(aes(colour = net_change_mha), alpha = 0.75, size = 1.8) +
  geom_text(aes(label = label_country), check_overlap = TRUE, size = 2.7,
            na.rm = TRUE,
            nudge_y = 0.01 * max_axis) +
  facet_wrap(vars(land_class), scales = "free", ncol = 3) +
  scale_colour_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                         midpoint = 0, name = "Net change\n(million ha)") +
  labs(
    title = "Countries below the diagonal lose more land than they regain",
    subtitle = "Protection after both CDR options: gross losses on the x-axis and gains on the y-axis",
    x = "Area lost from grid cells (million ha)",
    y = "Area gained in other grid cells (million ha)",
    caption = "Country labels mark the largest gross redistributions. Cell-centre country assignment is exploratory."
  ) + story_theme + theme(legend.position = "bottom")
ggsave(file.path(pushout_root, paste0("country_protection_loss_vs_gain_", target_year, ".png")),
       p, width = 13, height = 5.7, dpi = 300, bg = "white")

# Ratio above 100% means that gains within the same country exceed losses;
# below 100% means that the country does not recover all area lost from cells.
country_recovery <- country_grid_long %>%
  filter(comparison == "protection_with_both_cdr") %>%
  mutate(recovery_pct = if_else(loss > 1e-10, 100 * gain / loss, NA_real_))
mapped_recovery <- countries_sf %>% inner_join(country_recovery, by = "country")
p <- ggplot() +
  geom_sf(data = countries_sf, fill = "grey90", colour = "grey72", linewidth = 0.12) +
  geom_sf(data = mapped_recovery, aes(fill = recovery_pct),
          colour = "grey72", linewidth = 0.12) +
  facet_wrap(vars(land_class), ncol = 1) +
  scale_fill_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                       midpoint = 100, limits = c(0, 200), oob = squish,
                       na.value = "grey90", name = "Gain / loss\n(%)") +
  coord_sf(expand = FALSE) +
  labs(
    title = "How much land lost after protection is regained within each country?",
    subtitle = paste0(
      "100% means gross gains in other cells equal gross losses.\n",
      "Red indicates incomplete replacement; blue indicates net expansion."
    ),
    caption = "Protection after both CDR options (111−011). Grey countries have no measurable loss or no matched grid value."
  ) + theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10.5, colour = "grey30"),
        plot.caption = element_text(size = 8.5, colour = "grey35", hjust = 0),
        strip.text = element_text(face = "bold"), legend.position = "bottom")
ggsave(file.path(pushout_root, paste0("country_protection_recovery_ratio_maps_", target_year, ".png")),
       p, width = 10.5, height = 10.5, dpi = 300, bg = "white")

# Direct overlap evidence versus response evidence --------------------------

overlap_file <- file.path(output_dir, "08_land_use_protection",
                          "03_protected_footprint", "world",
                          paste0("world_protected_overlap_", target_year, ".csv"))
movement_file <- file.path(output_dir, "08_land_use_protection",
                           "02_protection_displacement", "world",
                           paste0("world_land_displacement_", target_year, ".csv"))
if (file.exists(overlap_file) && file.exists(movement_file)) {
  overlap <- read_csv(overlap_file, show_col_types = FALSE) %>%
    filter(state == "111")
  response <- read_csv(movement_file, show_col_types = FALSE) %>%
    filter(land_class %in% c("Food cropland", "2G bioenergy cropland", "Afforestation"))

  p_overlap <- ggplot(overlap, aes(protected_area_mha, component, fill = component)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = sprintf("%.0f", protected_area_mha)), hjust = -0.1, size = 3) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.14))) +
    labs(title = "Direct protected-area composition",
         subtitle = "Available for forest, grassland, cropland and pasture",
         x = "Protected-area output (million ha)", y = NULL) + story_theme

  p_response <- ggplot(response,
                       aes(protection_under_mitigation_mha, land_class,
                           fill = protection_under_mitigation_mha > 0)) +
    geom_vline(xintercept = 0, colour = "grey40") + geom_col(width = 0.66) +
    geom_text(aes(label = sprintf("%+.1f", protection_under_mitigation_mha)),
              hjust = ifelse(response$protection_under_mitigation_mha > 0, -0.1, 1.1),
              size = 3) +
    scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
    labs(title = "Land response after protection",
         subtitle = "Afforestation and 2G bioenergy are response measures, not direct overlap",
         x = "111 minus 011 (million ha)", y = NULL) + story_theme

  combined <- p_overlap + p_response +
    plot_annotation(
      title = "What is directly observed as overlap, and what is inferred from land response",
      subtitle = "Current output does not identify the direct protection × afforestation or protection × 2G-bioenergy intersection",
      caption = "Cropland overlap is not separated into food, fallow and energy cropland. Negative raw protected-cropland values are retained and flagged in the CSV."
    )
  ggsave(file.path(evidence_root, paste0("world_overlap_vs_land_response_", target_year, ".png")),
         combined, width = 14, height = 6.4, dpi = 300, bg = "white")
}

rm(merged_raw, merged_wide, grid_by_state, area_layers)
gc()
