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

parse_args <- function(args) {
  defaults <- list(
    gdx_a = "../data/scenario_compare/IAMC_templatePlus.gdx",
    gdx_b = "../data/scenario_compare_aff/IAMC_templatePlus.gdx",
    gdx_c = "../data/scenario_compare_aff2/IAMC_templatePlus.gdx",
    label_a = "scenario_compare",
    label_b = "scenario_compare_aff",
    label_c = "scenario_compare_aff2",
    scenario = "SSP2_500C_CACN_DAC_3_NoCC",
    symbol = "IAMC_templatePlus",
    out_dir = "../output/scenario_compare_lines",
    run_name = "ssp2_dac3_compare_vs_aff"
  )

  if (length(args) == 0) {
    return(defaults)
  }

  i <- 1
  while (i <= length(args)) {
    key <- args[i]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unknown argument format: %s", key), call. = FALSE)
    }
    if (i == length(args)) {
      stop(sprintf("Missing value for argument: %s", key), call. = FALSE)
    }
    val <- args[i + 1]
    if (startsWith(val, "--")) {
      stop(sprintf("Missing value for argument: %s", key), call. = FALSE)
    }

    if (key == "--gdx_a") {
      defaults$gdx_a <- val
    } else if (key == "--gdx_b") {
      defaults$gdx_b <- val
    } else if (key == "--gdx_c") {
      defaults$gdx_c <- val
    } else if (key == "--label_a") {
      defaults$label_a <- val
    } else if (key == "--label_b") {
      defaults$label_b <- val
    } else if (key == "--label_c") {
      defaults$label_c <- val
    } else if (key == "--scenario") {
      defaults$scenario <- val
    } else if (key == "--symbol") {
      defaults$symbol <- val
    } else if (key == "--out_dir") {
      defaults$out_dir <- val
    } else if (key == "--run_name") {
      defaults$run_name <- val
    } else {
      stop(sprintf("Unknown argument: %s", key), call. = FALSE)
    }
    i <- i + 2
  }

  defaults
}

region_levels <- c(
  "BRA", "CAN", "CHN", "CIS", "IND", "JPN", "TUR", "USA", "XAF",
  "XE25", "XER", "XLM", "XME", "XNF", "XOC", "XSA", "XSE",
  "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF", "World"
)

script_dir <- get_script_dir()
args <- parse_args(commandArgs(trailingOnly = TRUE))

gdx_a <- normalizePath(file.path(script_dir, args$gdx_a), winslash = "/", mustWork = FALSE)
gdx_b <- normalizePath(file.path(script_dir, args$gdx_b), winslash = "/", mustWork = FALSE)
gdx_c <- normalizePath(file.path(script_dir, args$gdx_c), winslash = "/", mustWork = FALSE)
out_base <- normalizePath(file.path(script_dir, args$out_dir), winslash = "/", mustWork = FALSE)
run_dir <- file.path(out_base, safe_file_name(args$run_name))

if (!file.exists(gdx_a)) {
  stop(sprintf("File not found: %s", gdx_a), call. = FALSE)
}
if (!file.exists(gdx_b)) {
  stop(sprintf("File not found: %s", gdx_b), call. = FALSE)
}
if (!file.exists(gdx_c)) {
  stop(sprintf("File not found: %s", gdx_c), call. = FALSE)
}

message("Initializing gdxrrw...")
gdxrrw::igdx(silent = TRUE)

message("Loading A: ", gdx_a)
df_a <- gdxrrw::rgdx.param(gdx_a, args$symbol) %>%
  mutate(dataset = args$label_a)
message("Loading B: ", gdx_b)
df_b <- gdxrrw::rgdx.param(gdx_b, args$symbol) %>%
  mutate(dataset = args$label_b)
message("Loading C: ", gdx_c)
df_c <- gdxrrw::rgdx.param(gdx_c, args$symbol) %>%
  mutate(dataset = args$label_c)

unit_a <- tryCatch(gdxrrw::rgdx.set(gdx_a, "VUMAP"), error = function(e) NULL)
unit_b <- tryCatch(gdxrrw::rgdx.set(gdx_b, "VUMAP"), error = function(e) NULL)
unit_c <- tryCatch(gdxrrw::rgdx.set(gdx_c, "VUMAP"), error = function(e) NULL)

required_cols <- c("Scenario", "REMF", "VEMF", "YEMF", args$symbol, "dataset")
if (length(setdiff(required_cols, names(df_a))) > 0) {
  stop("Missing required columns in first GDX.", call. = FALSE)
}
if (length(setdiff(required_cols, names(df_b))) > 0) {
  stop("Missing required columns in second GDX.", call. = FALSE)
}
if (length(setdiff(required_cols, names(df_c))) > 0) {
  stop("Missing required columns in third GDX.", call. = FALSE)
}

scen_a <- unique(as.character(df_a$Scenario))
scen_b <- unique(as.character(df_b$Scenario))
scen_c <- unique(as.character(df_c$Scenario))
if (!(args$scenario %in% scen_a)) {
  stop(sprintf("Scenario not found in A: %s", args$scenario), call. = FALSE)
}
if (!(args$scenario %in% scen_b)) {
  stop(sprintf("Scenario not found in B: %s", args$scenario), call. = FALSE)
}
if (!(args$scenario %in% scen_c)) {
  stop(sprintf("Scenario not found in C: %s", args$scenario), call. = FALSE)
}

all_df <- bind_rows(df_a, df_b, df_c) %>%
  mutate(
    Scenario = as.character(Scenario),
    REMF = as.character(REMF),
    VEMF = as.character(VEMF),
    YEMF = suppressWarnings(as.integer(as.character(YEMF)))
  ) %>%
  filter(Scenario == args$scenario, REMF %in% region_levels) %>%
  rename(value = all_of(args$symbol)) %>%
  mutate(
    value = as.numeric(value),
    Region = factor(REMF, levels = region_levels),
    dataset = factor(dataset, levels = c(args$label_a, args$label_b, args$label_c))
  ) %>%
  filter(!is.na(YEMF), !is.na(value), !is.na(VEMF), !is.na(Region))

if (nrow(all_df) == 0) {
  stop("No rows after filtering.", call. = FALSE)
}

unit_map <- NULL
collect_unit <- function(x) {
  if (is.data.frame(x) && all(c("VEMF", "UEMF") %in% names(x))) {
    x %>%
      mutate(VEMF = as.character(VEMF), UEMF = as.character(UEMF)) %>%
      distinct(VEMF, .keep_all = TRUE) %>%
      select(VEMF, UEMF)
  } else {
    NULL
  }
}
u1 <- collect_unit(unit_a)
u2 <- collect_unit(unit_b)
u3 <- collect_unit(unit_c)
unit_map <- bind_rows(u1, u2, u3) %>% distinct(VEMF, .keep_all = TRUE)
if (nrow(unit_map) == 0) {
  unit_map <- NULL
}

dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
vemf_list <- sort(unique(all_df$VEMF))
message("Rows after filter: ", nrow(all_df))
message("VEMF count: ", length(vemf_list))
message("Output directory: ", run_dir)

written <- 0
for (v in vemf_list) {
  df_v <- all_df %>%
    filter(VEMF == v) %>%
    arrange(Region, dataset, YEMF)

  if (nrow(df_v) == 0) {
    next
  }

  df_line <- df_v %>%
    group_by(Region, dataset) %>%
    filter(n() >= 2) %>%
    ungroup()

  y_label <- args$symbol
  if (!is.null(unit_map)) {
    unit_val <- unit_map$UEMF[unit_map$VEMF == v]
    if (length(unit_val) > 0 && !is.na(unit_val[1]) && nzchar(unit_val[1])) {
      y_label <- paste0(args$symbol, " [", unit_val[1], "]")
    }
  }

  p <- ggplot(df_v, aes(x = YEMF, y = value, color = dataset)) +
    geom_line(data = df_line, aes(group = dataset), linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.5, na.rm = TRUE) +
    facet_wrap(~Region, ncol = 6, scales = "free_y") +
    scale_color_manual(
      values = c(
        setNames("#1f77b4", args$label_a),
        setNames("#d62728", args$label_b),
        setNames("#2ca02c", args$label_c)
      ),
      drop = FALSE
    ) +
    scale_x_continuous(breaks = sort(unique(df_v$YEMF))) +
    labs(
      title = paste0("Scenario Comparison: ", v),
      subtitle = paste0(args$scenario, " | ", args$label_a, " vs ", args$label_b, " vs ", args$label_c),
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

  out_file <- file.path(run_dir, paste0(safe_file_name(v), ".png"))
  ggsave(filename = out_file, plot = p, width = 18, height = 10, dpi = 150)
  written <- written + 1
}

message("Finished. Files written: ", written)
