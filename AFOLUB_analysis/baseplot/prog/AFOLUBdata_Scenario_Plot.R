#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
})

# ------------------------------------------------------------
# User settings
# Edit this list to select countries to plot.
Country_list <- c(
  "USA", "CAN", "JPN", "DEU", "CHN", "BRA",
  "IND", "IDN", "MEX", "ZAF", "NGA", "ETH",
  "VNM", "TUR", "AUS", "GBR", "FRA", "RUS"
)
# ------------------------------------------------------------

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

plot_and_save <- function(df, scenarios, countries, out_file, title_text, y_label) {
  if (nrow(df) == 0) {
    return(FALSE)
  }

  df <- df %>% arrange(cty, SCE_AFOLUB, yr)
  df_line <- df %>%
    group_by(cty, SCE_AFOLUB) %>%
    filter(n() >= 2) %>%
    ungroup()

  color_values <- grDevices::hcl.colors(length(scenarios), palette = "Dark 3")
  names(color_values) <- scenarios

  p <- ggplot(df, aes(x = yr, y = value, color = SCE_AFOLUB)) +
    geom_line(data = df_line, aes(group = SCE_AFOLUB), linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.2, na.rm = TRUE) +
    facet_wrap(~cty, ncol = 6, scales = "free_y") +
    scale_color_manual(values = color_values, drop = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    labs(
      title = title_text,
      subtitle = paste(scenarios, collapse = " | "),
      x = "Year",
      y = y_label,
      color = "Scenario"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    filename = out_file,
    plot = p,
    width = 18,
    height = 10,
    dpi = 150
  )
  TRUE
}

script_dir <- get_script_dir()
gdx_path <- normalizePath(
  file.path(script_dir, "../data/AFOLUBdata/AFOLUBdata.gdx"),
  winslash = "/",
  mustWork = FALSE
)
reference_gdx_path <- normalizePath(
  file.path(script_dir, "../data/AFOLUBdata/SSP2_500C_NoCC_World.gdx"),
  winslash = "/",
  mustWork = FALSE
)
reference_scenario_name <- "SSP2_500C_NoCC_World"
out_base_dir <- normalizePath(
  file.path(script_dir, "../output/AFOLUBdata_variable_lines"),
  winslash = "/",
  mustWork = FALSE
)

if (!file.exists(gdx_path)) {
  stop(sprintf("GDX file not found: %s", gdx_path), call. = FALSE)
}

message("Initializing gdxrrw...")
gdxrrw::igdx(silent = TRUE)

symbols <- c("RR", "AC", "Pcb", "GHG")
raw_list <- list()

for (sym in symbols) {
  message("Loading symbol: ", sym)
  raw_list[[sym]] <- gdxrrw::rgdx.param(gdx_path, sym)
}

# Load optional reference scenario from separate world file.
if (file.exists(reference_gdx_path)) {
  message("Loading reference file: ", reference_gdx_path)
  ref_rr <- gdxrrw::rgdx.param(reference_gdx_path, "RR_load") %>%
    transmute(
      SCE_AFOLUB = reference_scenario_name,
      a = as.character(c),
      src = as.character(src),
      gas = as.character(gas),
      cty = as.character(cty),
      yr = as.character(Y),
      RR = RR_load
    )
  ref_ac <- gdxrrw::rgdx.param(reference_gdx_path, "AC_load") %>%
    transmute(
      SCE_AFOLUB = reference_scenario_name,
      a = as.character(c),
      cty = as.character(cty),
      yr = as.character(Y),
      AC = AC_load
    )
  ref_pcb <- gdxrrw::rgdx.param(reference_gdx_path, "Pcb_load") %>%
    transmute(
      SCE_AFOLUB = reference_scenario_name,
      a = as.character(c),
      cty = as.character(cty),
      yr = as.character(Y),
      Pcb = Pcb_load
    )
  ref_ghg <- gdxrrw::rgdx.param(reference_gdx_path, "GHG_load") %>%
    transmute(
      SCE_AFOLUB = reference_scenario_name,
      a = as.character(c),
      src = as.character(src),
      gas = as.character(gas),
      cty = as.character(cty),
      yr = as.character(Y),
      GHG = GHG_load
    )

  raw_list[["RR"]] <- bind_rows(raw_list[["RR"]], ref_rr)
  raw_list[["AC"]] <- bind_rows(raw_list[["AC"]], ref_ac)
  raw_list[["Pcb"]] <- bind_rows(raw_list[["Pcb"]], ref_pcb)
  raw_list[["GHG"]] <- bind_rows(raw_list[["GHG"]], ref_ghg)
} else {
  message("Reference file not found (skip): ", reference_gdx_path)
}

all_scenarios <- sort(unique(as.character(raw_list[["RR"]][["SCE_AFOLUB"]])))
all_countries <- sort(unique(as.character(raw_list[["RR"]][["cty"]])))
selected_countries <- Country_list[Country_list %in% all_countries]
missing_countries <- setdiff(Country_list, selected_countries)

if (length(selected_countries) == 0) {
  stop("No countries from Country_list were found in cty.", call. = FALSE)
}
if (length(missing_countries) > 0) {
  message("Countries not found and skipped: ", paste(missing_countries, collapse = ", "))
}

message("Scenarios found (SCE_AFOLUB): ", paste(all_scenarios, collapse = ", "))
message("Selected countries: ", paste(selected_countries, collapse = ", "))

dir.create(out_base_dir, recursive = TRUE, showWarnings = FALSE)

written <- 0

# RR and GHG: have gas and src, save as variable/gas/a_src.png
for (sym in c("RR", "GHG")) {
  df_sym <- raw_list[[sym]] %>%
    mutate(
      SCE_AFOLUB = as.character(SCE_AFOLUB),
      a = as.character(a),
      src = as.character(src),
      gas = as.character(gas),
      cty = as.character(cty),
      yr = suppressWarnings(as.integer(as.character(yr))),
      value = as.numeric(.data[[sym]])
    ) %>%
    filter(cty %in% selected_countries) %>%
    filter(!is.na(yr), !is.na(value))

  gas_list <- sort(unique(df_sym$gas))
  pair_list <- df_sym %>% distinct(a, src) %>% arrange(a, src)

  for (g in gas_list) {
    gas_dir <- file.path(out_base_dir, sym, safe_file_name(g))
    dir.create(gas_dir, recursive = TRUE, showWarnings = FALSE)

    for (i in seq_len(nrow(pair_list))) {
      a_i <- pair_list$a[i]
      src_i <- pair_list$src[i]
      df_plot <- df_sym %>%
        filter(gas == g, a == a_i, src == src_i) %>%
        mutate(
          SCE_AFOLUB = factor(SCE_AFOLUB, levels = all_scenarios),
          cty = factor(cty, levels = selected_countries)
        )

      out_file <- file.path(gas_dir, paste0(safe_file_name(paste(a_i, src_i, sep = "_")), ".png"))
      ok <- plot_and_save(
        df = df_plot,
        scenarios = all_scenarios,
        countries = selected_countries,
        out_file = out_file,
        title_text = paste0(sym, ": gas=", g, " | ", a_i, "_", src_i),
        y_label = sym
      )
      if (ok) {
        written <- written + 1
      }
    }
  }
}

# AC and Pcb: no gas/src dimension, save as variable/NA/a_all.png
for (sym in c("AC", "Pcb")) {
  df_sym <- raw_list[[sym]] %>%
    mutate(
      SCE_AFOLUB = as.character(SCE_AFOLUB),
      a = as.character(a),
      cty = as.character(cty),
      yr = suppressWarnings(as.integer(as.character(yr))),
      value = as.numeric(.data[[sym]])
    ) %>%
    filter(cty %in% selected_countries) %>%
    filter(!is.na(yr), !is.na(value))

  a_list <- sort(unique(df_sym$a))
  gas_dir <- file.path(out_base_dir, sym, "NA")
  dir.create(gas_dir, recursive = TRUE, showWarnings = FALSE)

  for (a_i in a_list) {
    df_plot <- df_sym %>%
      filter(a == a_i) %>%
      mutate(
        SCE_AFOLUB = factor(SCE_AFOLUB, levels = all_scenarios),
        cty = factor(cty, levels = selected_countries)
      )

    out_file <- file.path(gas_dir, paste0(safe_file_name(paste(a_i, "all", sep = "_")), ".png"))
    ok <- plot_and_save(
      df = df_plot,
      scenarios = all_scenarios,
      countries = selected_countries,
      out_file = out_file,
      title_text = paste0(sym, ": ", a_i),
      y_label = sym
    )
    if (ok) {
      written <- written + 1
    }
  }
}

message("Finished. Files written: ", written)
message("Output directory: ", out_base_dir)
