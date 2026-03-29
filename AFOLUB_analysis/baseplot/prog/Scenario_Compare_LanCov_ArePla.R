#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  getwd()
}

safe_file_name <- function(x) {
  y <- gsub("[^A-Za-z0-9._-]", "_", x)
  y <- gsub("_+", "_", y)
  y <- sub("^_", "", y)
  y <- sub("_$", "", y)
  ifelse(nchar(y) == 0, "unknown", y)
}

region_levels <- c(
  "BRA", "CAN", "CHN", "CIS", "IND", "JPN", "TUR", "USA", "XAF",
  "XE25", "XER", "XLM", "XME", "XNF", "XOC", "XSA", "XSE",
  "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF", "World"
)

script_dir <- get_script_dir()
gdx_a <- normalizePath(file.path(script_dir, "../data/scenario_compare/IAMC_templatePlus.gdx"), winslash = "/", mustWork = FALSE)
gdx_b <- normalizePath(file.path(script_dir, "../data/scenario_compare_aff/IAMC_templatePlus.gdx"), winslash = "/", mustWork = FALSE)
gdx_c <- normalizePath(file.path(script_dir, "../data/scenario_compare_aff/IAMC_SSP2_500C_afolub_2_NoCC_World.gdx"), winslash = "/", mustWork = FALSE)
out_dir <- normalizePath(file.path(script_dir, "../output/scenario_compare_lines/lan_cov_frs_compare_3way"), winslash = "/", mustWork = FALSE)

for (p in c(gdx_a, gdx_b, gdx_c)) {
  if (!file.exists(p)) {
    stop(sprintf("File not found: %s", p), call. = FALSE)
  }
}

message("Initializing gdxrrw...")
gdxrrw::igdx(silent = TRUE)

# A/B: IAMC_templatePlus
df_a <- gdxrrw::rgdx.param(gdx_a, "IAMC_templatePlus") %>%
  transmute(
    dataset = "scenario_compare",
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = suppressWarnings(as.integer(as.character(YEMF))),
    value = as.numeric(IAMC_templatePlus)
  )
df_b <- gdxrrw::rgdx.param(gdx_b, "IAMC_templatePlus") %>%
  transmute(
    dataset = "scenario_compare_aff",
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = suppressWarnings(as.integer(as.character(YEMF))),
    value = as.numeric(IAMC_templatePlus)
  )

# C: IAMC_Template
df_c <- gdxrrw::rgdx.param(gdx_c, "IAMC_Template") %>%
  transmute(
    dataset = "afolub_2_world",
    scenario = as.character(SCENARIO),
    region = as.character(IAMC_REGION),
    variable = as.character(IAMC_VAR),
    year = suppressWarnings(as.integer(as.character(Y))),
    value = as.numeric(IAMC_Template)
  )

target <- bind_rows(
  df_a %>% filter(scenario == "SSP2_500C_CACN_DAC_3_NoCC", variable == "Lan_Cov_Frs"),
  df_b %>% filter(scenario == "SSP2_500C_CACN_DAC_3_NoCC", variable == "Lan_Cov_Frs"),
  df_c %>% filter(scenario == "SSP2_500C_afolub_2_NoCC", variable == "Lan_Cov_Frs")
) %>%
  filter(region %in% region_levels) %>%
  filter(!is.na(year), !is.na(value)) %>%
  mutate(
    region = factor(region, levels = region_levels),
    dataset = factor(dataset, levels = c("scenario_compare", "scenario_compare_aff", "afolub_2_world"))
  )

if (nrow(target) == 0) {
  stop("No rows found for Lan_Cov_Frs with target scenarios.", call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

color_values <- c(
  "scenario_compare" = "#1f77b4",
  "scenario_compare_aff" = "#d62728",
  "afolub_2_world" = "#2ca02c"
)

# 1) Lan_Cov_Frs level comparison
line_df <- target %>%
  group_by(region, dataset) %>%
  filter(n() >= 2) %>%
  ungroup()

p1 <- ggplot(target, aes(x = year, y = value, color = dataset)) +
  geom_line(data = line_df, aes(group = dataset), linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  facet_wrap(~region, ncol = 6, scales = "free_y") +
  scale_color_manual(values = color_values, drop = FALSE) +
  scale_x_continuous(breaks = sort(unique(target$year))) +
  labs(
    title = "Lan_Cov_Frs Comparison (3 datasets)",
    subtitle = "scenario_compare / scenario_compare_aff: SSP2_500C_CACN_DAC_3_NoCC | afolub_2_world: SSP2_500C_afolub_2_NoCC",
    x = "Year",
    y = "Lan_Cov_Frs",
    color = "Dataset"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = file.path(out_dir, "Lan_Cov_Frs.png"),
  plot = p1,
  width = 18,
  height = 10,
  dpi = 150
)

# 2) Lan_Are_Pla = year-to-year increase (annualized by year gap)
delta_df <- target %>%
  group_by(region, dataset) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    year_gap = year - lag(year),
    Lan_Are_Pla = (value - lag(value)) / year_gap
  ) %>%
  ungroup() %>%
  filter(!is.na(Lan_Are_Pla), !is.na(year_gap), year_gap > 0)

if (nrow(delta_df) > 0) {
  delta_line <- delta_df %>%
    group_by(region, dataset) %>%
    filter(n() >= 2) %>%
    ungroup()

  p2 <- ggplot(delta_df, aes(x = year, y = Lan_Are_Pla, color = dataset)) +
    geom_line(data = delta_line, aes(group = dataset), linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 1.8, na.rm = TRUE) +
    facet_wrap(~region, ncol = 6, scales = "free_y") +
    scale_color_manual(values = color_values, drop = FALSE) +
    scale_x_continuous(breaks = sort(unique(delta_df$year))) +
    labs(
      title = "Lan_Are_Pla Comparison (derived from Lan_Cov_Frs)",
      subtitle = "Lan_Are_Pla = (Lan_Cov_Frs[t] - Lan_Cov_Frs[t-1]) / (year[t] - year[t-1])",
      x = "Year",
      y = "Lan_Are_Pla (Lan_Cov_Frs unit/year)",
      color = "Dataset"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    filename = file.path(out_dir, "Lan_Are_Pla.png"),
    plot = p2,
    width = 18,
    height = 10,
    dpi = 150
  )
}

message("Finished. Output directory: ", out_dir)
