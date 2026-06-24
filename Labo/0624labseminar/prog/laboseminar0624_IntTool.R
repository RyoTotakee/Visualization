library(tidyverse)
library(gdxrrw)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
gams_sys_dir <- if (length(args) >= 1 && dir.exists(args[1])) args[1] else Sys.getenv("GAMS_SYS_DIR", "")
if (nzchar(gams_sys_dir)) {
  igdx(gams_sys_dir)
}

script_arg <- commandArgs(FALSE)
script_file <- sub("^--file=", "", script_arg[grepl("^--file=", script_arg)])
script_dir <- if (length(script_file) > 0) {
  dirname(normalizePath(script_file[1], winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data", "IntTool")
output_dir <- file.path(project_dir, "Output", "IntTool")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

agmip_gdx <- file.path(data_dir, "AGMIP_all_scenario.gdx")
if (!file.exists(agmip_gdx)) {
  stop("Missing required data file: ", agmip_gdx)
}

all_scenarios <- c(
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global2",
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global3",
  "SSP2_1100C_CACN_DAC_NoCC_No",
  "SSP2_BaU_NoCC_No"
)

plot_scenarios <- c(
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global2",
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global3",
  "SSP2_BaU_NoCC_No"
)

scenario_labels <- c(
  SSP2_1100C_CACN_DAC_NoCC_LandInt_global1 = "Ite1",
  SSP2_1100C_CACN_DAC_NoCC_LandInt_global2 = "Ite2",
  SSP2_1100C_CACN_DAC_NoCC_LandInt_global3 = "Ite3",
  SSP2_1100C_CACN_DAC_NoCC_No = "No LandInt",
  SSP2_BaU_NoCC_No = "BaU"
)

scenario_colors <- c(
  "Ite1" = "#1B9E77",
  "Ite2" = "#D95F02",
  "Ite3" = "#7570B3",
  "No LandInt" = "#E7298A",
  "BaU" = "#303030"
)

scenario_linetypes <- c(
  "Ite1" = "solid",
  "Ite2" = "solid",
  "Ite3" = "solid",
  "No LandInt" = "solid",
  "BaU" = "dotted"
)

target_vars <- tribble(
  ~AgMIP_VAR,         ~ITEM, ~AgMIP_UNIT, ~PANEL_LABEL,                         ~TABLE_LABEL,                  ~VALUE_SCALE,
  "ECH4",            "AGR", "MtCO2e",    "CH4\nemissions\n[MtCO2e]",          "CH4 emissions",               1,
  "EN2O",            "AGR", "MtCO2e",    "N2O\nemissions\n[MtCO2e]",          "N2O emissions",               1,
  "EmisRedRateCH4",  "AGR", "%",         "CH4 emission\nreduction rate\n[%]", "CH4 emission reduction rate", 1,
  "EmisRedRateN2O",  "AGR", "%",         "N2O emission\nreduction rate\n[%]", "N2O emission reduction rate", 1,
  "PROD",            "CRP", "1000 t",    "Crop\nproduction\n[million t]",     "Crop production",             1000,
  "PROD",            "LSP", "1000 t",    "Livestock\nproduction\n[million t]", "Livestock production",        1000
) %>%
  mutate(PANEL_LABEL = factor(PANEL_LABEL, levels = PANEL_LABEL))

r5_region_labels <- c(
  "R5ASIA" = "Asia",
  "R5LAM" = "Latin America and\nthe Caribbean",
  "R5MAF" = "Middle East and\nAfrica",
  "R5OECD90+EU" = "OECD+EU",
  "R5REF" = "Eastern Europe and\nthe Former Soviet Union",
  "R5.2ASIA" = "Asia",
  "R5.2LAM" = "Latin America and\nthe Caribbean",
  "R5.2MAF" = "Middle East and\nAfrica",
  "R5.2OECD" = "OECD+EU",
  "R5.2REF" = "Eastern Europe and\nthe Former Soviet Union"
)

r5_region_order <- c(
  "Asia",
  "Latin America and\nthe Caribbean",
  "Middle East and\nAfrica",
  "OECD+EU",
  "Eastern Europe and\nthe Former Soviet Union"
)

theme_inttool <- theme_bw() +
  theme(
    text = element_text(size = 18),
    axis.text = element_text(size = 13),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.key.width = unit(1.4, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 15),
    panel.grid.minor = element_blank(),
    plot.margin = margin(8, 12, 8, 8)
  )

theme_inttool_matrix <- theme_inttool +
  theme(
    text = element_text(size = 13),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.2, "cm"),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10),
    panel.spacing = unit(0.35, "lines")
  )

message("Reading AGMIP_agg from: ", agmip_gdx)
agmip_agg <- as_tibble(rgdx.param(agmip_gdx, "AGMIP_agg")) %>%
  mutate(
    across(c(SCENARIO, region, AgMIP_VAR, ITEM, AgMIP_UNIT), as.character),
    Y = as.numeric(as.character(Y))
  )

missing_scenarios <- setdiff(all_scenarios, unique(agmip_agg$SCENARIO))
if (length(missing_scenarios) > 0) {
  warning("Missing scenarios in AGMIP_agg:\n", paste(missing_scenarios, collapse = "\n"))
}

plot_data <- agmip_agg %>%
  filter(
    SCENARIO %in% plot_scenarios,
    region == "World",
    Y >= 2015,
    Y <= 2100
  ) %>%
  inner_join(target_vars, by = c("AgMIP_VAR", "ITEM", "AgMIP_UNIT")) %>%
  mutate(
    PLOT_VALUE = AGMIP_agg / VALUE_SCALE,
    SCENARIO_LABEL = unname(scenario_labels[SCENARIO]),
    SCENARIO_LABEL = factor(SCENARIO_LABEL, levels = unname(scenario_labels[plot_scenarios]))
  )

if (nrow(plot_data) == 0) {
  stop("No rows found for the requested IntTool plot.")
}

missing_panels <- setdiff(levels(target_vars$PANEL_LABEL), unique(as.character(plot_data$PANEL_LABEL)))
if (length(missing_panels) > 0) {
  warning("Missing panels in plot data:\n", paste(missing_panels, collapse = "\n"))
}

p_world <- ggplot(
  plot_data,
  aes(x = Y, y = PLOT_VALUE, color = SCENARIO_LABEL, linetype = SCENARIO_LABEL)
) +
  geom_line(linewidth = 1.05) +
  facet_wrap(vars(PANEL_LABEL), scales = "free_y", ncol = 3) +
  scale_color_manual(
    values = scenario_colors,
    breaks = unname(scenario_labels[plot_scenarios])
  ) +
  scale_linetype_manual(
    values = scenario_linetypes,
    breaks = unname(scenario_labels[plot_scenarios])
  ) +
  scale_x_continuous(breaks = seq(2020, 2100, 20), limits = c(2015, 2100)) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  theme_inttool

output_png <- file.path(output_dir, "IntTool_World_6panel.png")
ggsave(output_png, p_world, width = 14, height = 8, dpi = 300)
message("Saved: ", output_png)

r5_data <- agmip_agg %>%
  filter(
    SCENARIO %in% plot_scenarios,
    region %in% names(r5_region_labels),
    Y >= 2015,
    Y <= 2100
  ) %>%
  inner_join(target_vars, by = c("AgMIP_VAR", "ITEM", "AgMIP_UNIT")) %>%
  mutate(
    PLOT_VALUE = AGMIP_agg / VALUE_SCALE,
    SCENARIO_LABEL = unname(scenario_labels[SCENARIO]),
    SCENARIO_LABEL = factor(SCENARIO_LABEL, levels = unname(scenario_labels[plot_scenarios])),
    REGION_LABEL = unname(r5_region_labels[region]),
    REGION_LABEL = factor(REGION_LABEL, levels = r5_region_order)
  )

if (nrow(r5_data) == 0) {
  warning("No rows found for the requested IntTool R5 plot.")
} else {
  missing_r5_regions <- setdiff(r5_region_order, unique(as.character(r5_data$REGION_LABEL)))
  if (length(missing_r5_regions) > 0) {
    warning("Missing R5 regions in plot data:\n", paste(missing_r5_regions, collapse = "\n"))
  }

  p_r5 <- ggplot(
    r5_data,
    aes(x = Y, y = PLOT_VALUE, color = SCENARIO_LABEL, linetype = SCENARIO_LABEL)
  ) +
    geom_line(linewidth = 0.75) +
    ggh4x::facet_grid2(
      rows = vars(PANEL_LABEL),
      cols = vars(REGION_LABEL),
      scales = "free",
      independent = "y"
    ) +
    scale_color_manual(
      values = scenario_colors,
      breaks = unname(scenario_labels[plot_scenarios])
    ) +
    scale_linetype_manual(
      values = scenario_linetypes,
      breaks = unname(scenario_labels[plot_scenarios])
    ) +
    scale_x_continuous(breaks = seq(2020, 2100, 40), limits = c(2015, 2100)) +
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE),
      linetype = guide_legend(nrow = 1, byrow = TRUE)
    ) +
    theme_inttool_matrix

  output_r5_png <- file.path(output_dir, "IntTool_R5_6indicator_matrix.png")
  ggsave(output_r5_png, p_r5, width = 12, height = 8, dpi = 300)
  message("Saved: ", output_r5_png)
}

iteration_scenarios <- c(
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global2",
  "SSP2_1100C_CACN_DAC_NoCC_LandInt_global3"
)

convergence_pair_table <- tribble(
  ~PAIR,          ~BASE, ~NEXT,
  "Ite1_to_Ite2", "Ite1", "Ite2",
  "Ite2_to_Ite3", "Ite2", "Ite3"
)

classify_convergence <- function(gap12, gap23, ratio) {
  case_when(
    is.na(gap12) | is.na(gap23) ~ "No data",
    gap23 <= 0.5 ~ "Converged",
    gap12 <= 0.5 & gap23 <= 1.0 ~ "Already close",
    !is.na(ratio) & ratio <= 0.25 ~ "Converged",
    !is.na(ratio) & ratio <= 0.50 ~ "Mostly converged",
    !is.na(ratio) & ratio < 1.00 ~ "Improving",
    TRUE ~ "Not converged"
  )
}

summarise_convergence <- function(df, level_col, entity_col, value_col) {
  wide_data <- df %>%
    filter(
      SCENARIO %in% iteration_scenarios,
      Y >= 2015,
      Y <= 2100
    ) %>%
    inner_join(target_vars, by = c("AgMIP_VAR", "ITEM", "AgMIP_UNIT")) %>%
    mutate(
      LEVEL = .data[[level_col]],
      ENTITY = .data[[entity_col]],
      INDICATOR = TABLE_LABEL,
      UNIT = str_extract(as.character(PANEL_LABEL), "\\[[^\\]]+\\]") %>% str_remove_all("\\[|\\]"),
      SCENARIO_LABEL = unname(scenario_labels[SCENARIO]),
      VALUE = .data[[value_col]] / VALUE_SCALE
    ) %>%
    select(LEVEL, ENTITY, INDICATOR, UNIT, Y, SCENARIO_LABEL, VALUE) %>%
    pivot_wider(names_from = SCENARIO_LABEL, values_from = VALUE)

  pair_data <- convergence_pair_table %>%
    pmap_dfr(function(PAIR, BASE, NEXT) {
      wide_data %>%
        transmute(
          LEVEL,
          ENTITY,
          INDICATOR,
          UNIT,
          Y,
          PAIR,
          BASE_VALUE = .data[[BASE]],
          NEXT_VALUE = .data[[NEXT]]
        )
    }) %>%
    filter(!is.na(BASE_VALUE), !is.na(NEXT_VALUE)) %>%
    mutate(
      ABS_DIFF = abs(NEXT_VALUE - BASE_VALUE),
      SIGNED_DIFF = NEXT_VALUE - BASE_VALUE,
      SMAPE_PCT = if_else(
        abs(BASE_VALUE) + abs(NEXT_VALUE) < 1e-12,
        0,
        200 * ABS_DIFF / (abs(BASE_VALUE) + abs(NEXT_VALUE))
      )
    )

  pair_summary <- pair_data %>%
    group_by(LEVEL, ENTITY, INDICATOR, UNIT, PAIR) %>%
    summarise(
      n_years = n(),
      mean_abs_diff = mean(ABS_DIFF, na.rm = TRUE),
      max_abs_diff = max(ABS_DIFF, na.rm = TRUE),
      mean_smape_pct = mean(SMAPE_PCT, na.rm = TRUE),
      max_smape_pct = max(SMAPE_PCT, na.rm = TRUE),
      diff_2100 = SIGNED_DIFF[which.max(Y)],
      .groups = "drop"
    )

  pair_summary %>%
    pivot_wider(
      names_from = PAIR,
      values_from = c(n_years, mean_abs_diff, max_abs_diff, mean_smape_pct, max_smape_pct, diff_2100)
    ) %>%
    mutate(
      convergence_ratio = mean_smape_pct_Ite2_to_Ite3 / mean_smape_pct_Ite1_to_Ite2,
      convergence_ratio = if_else(is.infinite(convergence_ratio), NA_real_, convergence_ratio),
      judgement = classify_convergence(
        mean_smape_pct_Ite1_to_Ite2,
        mean_smape_pct_Ite2_to_Ite3,
        convergence_ratio
      )
    ) %>%
    arrange(LEVEL, ENTITY, INDICATOR)
}

message("Calculating convergence for World and R5...")
agg_convergence_input <- agmip_agg %>%
  filter(region == "World" | region %in% names(r5_region_labels)) %>%
  mutate(
    LEVEL = if_else(region == "World", "World", "R5"),
    ENTITY = if_else(region == "World", "World", unname(r5_region_labels[region]))
  )

agg_convergence <- summarise_convergence(
  agg_convergence_input,
  level_col = "LEVEL",
  entity_col = "ENTITY",
  value_col = "AGMIP_agg"
)

rm(agg_convergence_input, agmip_agg, plot_data, r5_data)
gc()

message("Reading country-level AGMIP from: ", agmip_gdx)
agmip_country <- as_tibble(rgdx.param(agmip_gdx, "AGMIP")) %>%
  mutate(
    across(c(SCENARIO, cty, AgMIP_VAR, ITEM, AgMIP_UNIT), as.character),
    Y = as.numeric(as.character(Y)),
    LEVEL = "Country",
    ENTITY = cty
  )

message("Calculating convergence for countries...")
country_convergence <- summarise_convergence(
  agmip_country,
  level_col = "LEVEL",
  entity_col = "ENTITY",
  value_col = "AGMIP"
)

rm(agmip_country)
gc()

convergence_detail <- bind_rows(agg_convergence, country_convergence)

round_convergence_table <- function(df) {
  df %>%
    mutate(
      across(contains("smape"), ~ round(.x, 4)),
      across(contains("gap"), ~ round(.x, 4)),
      across(contains("diff"), ~ round(.x, 4)),
      across(contains("ratio"), ~ round(.x, 4)),
      across(contains("share"), ~ round(.x, 4))
    )
}

convergence_summary <- convergence_detail %>%
  group_by(LEVEL, INDICATOR, UNIT) %>%
  summarise(
    n_entities = n(),
    median_gap_Ite1_to_Ite2_pct = median(mean_smape_pct_Ite1_to_Ite2, na.rm = TRUE),
    median_gap_Ite2_to_Ite3_pct = median(mean_smape_pct_Ite2_to_Ite3, na.rm = TRUE),
    median_convergence_ratio = median(convergence_ratio, na.rm = TRUE),
    p90_gap_Ite2_to_Ite3_pct = quantile(mean_smape_pct_Ite2_to_Ite3, 0.90, na.rm = TRUE),
    max_gap_Ite2_to_Ite3_pct = max(mean_smape_pct_Ite2_to_Ite3, na.rm = TRUE),
    share_converged_or_mostly = mean(judgement %in% c("Converged", "Already close", "Mostly converged"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(LEVEL, INDICATOR)

worst_countries <- convergence_detail %>%
  filter(LEVEL == "Country") %>%
  arrange(INDICATOR, desc(mean_smape_pct_Ite2_to_Ite3), desc(convergence_ratio)) %>%
  group_by(INDICATOR) %>%
  slice_head(n = 20) %>%
  ungroup()

convergence_detail <- round_convergence_table(convergence_detail)
convergence_summary <- round_convergence_table(convergence_summary)
worst_countries <- round_convergence_table(worst_countries)

detail_csv <- file.path(output_dir, "IntTool_convergence_detail.csv")
summary_csv <- file.path(output_dir, "IntTool_convergence_summary.csv")
worst_csv <- file.path(output_dir, "IntTool_convergence_worst_countries.csv")

write_csv(convergence_detail, detail_csv)
write_csv(convergence_summary, summary_csv)
write_csv(worst_countries, worst_csv)

message("Saved: ", detail_csv)
message("Saved: ", summary_csv)
message("Saved: ", worst_csv)
