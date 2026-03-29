#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
})

# ------------------------------------------------------------
# User settings
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

plot_and_save <- function(df, out_file, title_text, subtitle_text, y_label) {
  if (nrow(df) == 0) {
    return(FALSE)
  }

  df <- df %>% arrange(cty, dataset, yr)
  df_line <- df %>%
    group_by(cty, dataset) %>%
    filter(n() >= 2) %>%
    ungroup()

  p <- ggplot(df, aes(x = yr, y = value, color = dataset)) +
    geom_line(data = df_line, aes(group = dataset), linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.2, na.rm = TRUE) +
    facet_wrap(~cty, ncol = 6, scales = "free_y") +
    scale_color_manual(values = c(Old = "#1f77b4", New = "#d62728"), drop = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Year",
      y = y_label,
      color = "Dataset"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      strip.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(filename = out_file, plot = p, width = 18, height = 10, dpi = 150)
  TRUE
}

script_dir <- get_script_dir()
old_gdx <- normalizePath(
  file.path(script_dir, "../data/AFOLUBdata/AFOLUBdata_old.gdx"),
  winslash = "/",
  mustWork = FALSE
)
new_gdx <- normalizePath(
  file.path(script_dir, "../data/AFOLUBdata/AFOLUBdata_new.gdx"),
  winslash = "/",
  mustWork = FALSE
)
out_base <- normalizePath(
  file.path(script_dir, "../output/AFOLUBdata_old_new_lines"),
  winslash = "/",
  mustWork = FALSE
)

if (!file.exists(old_gdx)) {
  stop(sprintf("Old GDX file not found: %s", old_gdx), call. = FALSE)
}
if (!file.exists(new_gdx)) {
  stop(sprintf("New GDX file not found: %s", new_gdx), call. = FALSE)
}

message("Initializing gdxrrw...")
gdxrrw::igdx(silent = TRUE)

symbols <- c("RR", "AC", "GHG")
old <- list()
new <- list()
for (s in symbols) {
  message("Loading ", s, " from old/new...")
  old[[s]] <- gdxrrw::rgdx.param(old_gdx, s)
  new[[s]] <- gdxrrw::rgdx.param(new_gdx, s)
}

scenario_old <- sort(unique(as.character(old[["RR"]][["SCE_AFOLUB"]])))
scenario_new <- sort(unique(as.character(new[["RR"]][["SCE_AFOLUB"]])))
common_scenarios <- intersect(scenario_old, scenario_new)
if (length(common_scenarios) > 0) {
  scenario_pairs <- data.frame(
    old_sce = common_scenarios,
    new_sce = common_scenarios,
    out_sce = common_scenarios,
    stringsAsFactors = FALSE
  )
} else if (length(scenario_new) == 1) {
  scenario_pairs <- data.frame(
    old_sce = scenario_old,
    new_sce = scenario_new[1],
    out_sce = paste0(scenario_old, "__vs__", scenario_new[1]),
    stringsAsFactors = FALSE
  )
} else if (length(scenario_old) == 1) {
  scenario_pairs <- data.frame(
    old_sce = scenario_old[1],
    new_sce = scenario_new,
    out_sce = paste0(scenario_old[1], "__vs__", scenario_new),
    stringsAsFactors = FALSE
  )
} else {
  stop("No common SCE_AFOLUB between old and new, and no safe 1-to-many mapping found.", call. = FALSE)
}

country_old <- sort(unique(as.character(old[["RR"]][["cty"]])))
country_new <- sort(unique(as.character(new[["RR"]][["cty"]])))
countries_common <- intersect(country_old, country_new)
countries <- Country_list[Country_list %in% countries_common]
if (length(countries) == 0) {
  stop("No selected countries found in both old and new datasets.", call. = FALSE)
}

message("Scenario pairs:")
for (i in seq_len(nrow(scenario_pairs))) {
  message("  - old: ", scenario_pairs$old_sce[i], " | new: ", scenario_pairs$new_sce[i])
}
message("Selected countries: ", paste(countries, collapse = ", "))

dir.create(out_base, recursive = TRUE, showWarnings = FALSE)
written <- 0

# RR and GHG: variable/gas/scenario/a_src.png
for (sym in c("RR", "GHG")) {
  df_old <- old[[sym]] %>%
    mutate(
      SCE_AFOLUB = as.character(SCE_AFOLUB),
      a = as.character(a),
      src = as.character(src),
      gas = as.character(gas),
      cty = as.character(cty),
      yr = suppressWarnings(as.integer(as.character(yr))),
      value = as.numeric(.data[[sym]]),
      dataset = "Old"
    )
  df_new <- new[[sym]] %>%
    mutate(
      SCE_AFOLUB = as.character(SCE_AFOLUB),
      a = as.character(a),
      src = as.character(src),
      gas = as.character(gas),
      cty = as.character(cty),
      yr = suppressWarnings(as.integer(as.character(yr))),
      value = as.numeric(.data[[sym]]),
      dataset = "New"
    )

  df <- bind_rows(df_old, df_new) %>%
    filter(cty %in% countries) %>%
    filter(!is.na(yr), !is.na(value))

  gas_list <- sort(unique(df$gas))
  pair_list <- df %>% distinct(a, src) %>% arrange(a, src)

  for (g in gas_list) {
    for (j in seq_len(nrow(scenario_pairs))) {
      old_sce <- scenario_pairs$old_sce[j]
      new_sce <- scenario_pairs$new_sce[j]
      out_sce <- scenario_pairs$out_sce[j]
      out_dir <- file.path(out_base, sym, safe_file_name(g), safe_file_name(out_sce))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

      for (i in seq_len(nrow(pair_list))) {
        a_i <- pair_list$a[i]
        src_i <- pair_list$src[i]
        df_plot <- df %>%
          filter(
            gas == g,
            a == a_i,
            src == src_i,
            (dataset == "Old" & SCE_AFOLUB == old_sce) |
              (dataset == "New" & SCE_AFOLUB == new_sce)
          ) %>%
          mutate(
            cty = factor(cty, levels = countries),
            dataset = factor(dataset, levels = c("Old", "New"))
          )

        out_file <- file.path(out_dir, paste0(safe_file_name(paste(a_i, src_i, sep = "_")), ".png"))
        ok <- plot_and_save(
          df = df_plot,
          out_file = out_file,
          title_text = paste0(sym, ": gas=", g, " | ", a_i, "_", src_i),
          subtitle_text = paste0("Old=", old_sce, " | New=", new_sce),
          y_label = sym
        )
        if (ok) {
          written <- written + 1
        }
      }
    }
  }
}

# AC: variable/NA/scenario/a.png
{
  df_old <- old[["AC"]] %>%
    mutate(
      SCE_AFOLUB = as.character(SCE_AFOLUB),
      a = as.character(a),
      cty = as.character(cty),
      yr = suppressWarnings(as.integer(as.character(yr))),
      value = as.numeric(AC),
      dataset = "Old"
    )
  df_new <- new[["AC"]] %>%
    mutate(
      SCE_AFOLUB = as.character(SCE_AFOLUB),
      a = as.character(a),
      cty = as.character(cty),
      yr = suppressWarnings(as.integer(as.character(yr))),
      value = as.numeric(AC),
      dataset = "New"
    )
  df <- bind_rows(df_old, df_new) %>%
    filter(cty %in% countries) %>%
    filter(!is.na(yr), !is.na(value))

  a_list <- sort(unique(df$a))
  for (j in seq_len(nrow(scenario_pairs))) {
    old_sce <- scenario_pairs$old_sce[j]
    new_sce <- scenario_pairs$new_sce[j]
    out_sce <- scenario_pairs$out_sce[j]
    out_dir <- file.path(out_base, "AC", "NA", safe_file_name(out_sce))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    for (a_i in a_list) {
      df_plot <- df %>%
        filter(
          a == a_i,
          (dataset == "Old" & SCE_AFOLUB == old_sce) |
            (dataset == "New" & SCE_AFOLUB == new_sce)
        ) %>%
        mutate(
          cty = factor(cty, levels = countries),
          dataset = factor(dataset, levels = c("Old", "New"))
        )

      out_file <- file.path(out_dir, paste0(safe_file_name(a_i), ".png"))
      ok <- plot_and_save(
        df = df_plot,
        out_file = out_file,
        title_text = paste0("AC: ", a_i),
        subtitle_text = paste0("Old=", old_sce, " | New=", new_sce),
        y_label = "AC"
      )
      if (ok) {
        written <- written + 1
      }
    }
  }
}

message("Finished. Files written: ", written)
message("Output directory: ", out_base)
