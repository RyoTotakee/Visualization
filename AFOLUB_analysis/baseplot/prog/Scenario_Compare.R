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

trim_ws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

safe_file_name <- function(x) {
  y <- gsub("[^A-Za-z0-9._-]", "_", x)
  y <- gsub("_+", "_", y)
  y <- sub("^_", "", y)
  y <- sub("_$", "", y)
  ifelse(nchar(y) == 0, "unknown", y)
}

print_help <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript Scenario_Compare.R [options]",
      "",
      "Options:",
      "  --gdx_path   Input GDX path (default: ../data/scenario_compare/IAMC_templatePlus.gdx)",
      "  --scenarios  Comma separated scenario list",
      "               (default: SSP2_500C_CACN_DAC_NoCC,SSP2_500C_CACN_DAC_2_NoCC)",
      "  --symbol     GDX symbol name (default: IAMC_templatePlus)",
      "  --out_dir    Output root directory (default: ../output/scenario_compare_lines)",
      "  --run_name   Output subdirectory name (optional)",
      "  --help       Show this help",
      "",
      "Example:",
      "  Rscript Scenario_Compare.R \\",
      "    --scenarios \"SSP2_500C_CACN_DAC_NoCC,SSP2_500C_CACN_DAC_2_NoCC\"",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  defaults <- list(
    gdx_path = "../data/scenario_compare/IAMC_templatePlus.gdx",
    scenarios = "SSP2_500C_CACN_DAC_NoCC,SSP2_500C_CACN_DAC_2_NoCC",
    symbol = "IAMC_templatePlus",
    out_dir = "../output/scenario_compare_lines",
    run_name = NULL,
    help = FALSE
  )

  if (length(args) == 0) {
    return(defaults)
  }

  i <- 1
  while (i <= length(args)) {
    key <- args[i]
    if (key == "--help") {
      defaults$help <- TRUE
      i <- i + 1
      next
    }
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

    if (key == "--gdx_path") {
      defaults$gdx_path <- val
    } else if (key == "--scenarios") {
      defaults$scenarios <- val
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

parse_scenarios <- function(text) {
  scenarios <- unlist(strsplit(text, ",", fixed = TRUE))
  scenarios <- trim_ws(scenarios)
  scenarios <- scenarios[nchar(scenarios) > 0]
  if (length(scenarios) < 2) {
    stop("Please specify at least two scenarios in `--scenarios`.", call. = FALSE)
  }
  unique(scenarios)
}

region_levels <- c(
  "BRA", "CAN", "CHN", "CIS", "IND", "JPN", "TUR", "USA", "XAF",
  "XE25", "XER", "XLM", "XME", "XNF", "XOC", "XSA", "XSE",
  "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF", "World"
)

script_dir <- get_script_dir()
args <- parse_args(commandArgs(trailingOnly = TRUE))
if (isTRUE(args$help)) {
  print_help()
  quit(save = "no", status = 0)
}

target_scenarios <- parse_scenarios(args$scenarios)
gdx_path <- normalizePath(file.path(script_dir, args$gdx_path), winslash = "/", mustWork = FALSE)
out_base_dir <- normalizePath(file.path(script_dir, args$out_dir), winslash = "/", mustWork = FALSE)

if (!file.exists(gdx_path)) {
  stop(sprintf("GDX file not found: %s", gdx_path), call. = FALSE)
}

message("Initializing gdxrrw...")
gdxrrw::igdx(silent = TRUE)

message("Loading GDX: ", gdx_path)
raw_df <- gdxrrw::rgdx.param(gdx_path, args$symbol)
unit_df <- tryCatch(
  gdxrrw::rgdx.set(gdx_path, "VUMAP"),
  error = function(e) NULL
)
required_cols <- c("Scenario", "REMF", "VEMF", "YEMF", args$symbol)
missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

available_scenarios <- sort(unique(as.character(raw_df$Scenario)))
not_found <- setdiff(target_scenarios, available_scenarios)
if (length(not_found) > 0) {
  stop(
    sprintf(
      "Scenario not found: %s\nAvailable scenarios: %s",
      paste(not_found, collapse = ", "),
      paste(available_scenarios, collapse = ", ")
    ),
    call. = FALSE
  )
}

plot_df <- raw_df %>%
  mutate(
    Scenario = as.character(Scenario),
    REMF = as.character(REMF),
    VEMF = as.character(VEMF),
    YEMF = suppressWarnings(as.integer(as.character(YEMF)))
  ) %>%
  filter(Scenario %in% target_scenarios) %>%
  filter(REMF %in% region_levels) %>%
  rename(value = all_of(args$symbol)) %>%
  mutate(
    value = as.numeric(value),
    Region = factor(REMF, levels = region_levels),
    Scenario = factor(Scenario, levels = target_scenarios)
  ) %>%
  filter(!is.na(YEMF), !is.na(value), !is.na(VEMF), !is.na(Region))

if (nrow(plot_df) == 0) {
  stop("No data after filtering by scenarios and regions.", call. = FALSE)
}

run_name <- if (is.null(args$run_name) || args$run_name == "") {
  safe_file_name(paste(target_scenarios, collapse = "__"))
} else {
  safe_file_name(args$run_name)
}
run_dir <- file.path(out_base_dir, run_name)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

scenario_levels <- levels(plot_df$Scenario)
color_values <- grDevices::hcl.colors(length(scenario_levels), palette = "Dark 3")
names(color_values) <- scenario_levels

unit_map <- NULL
if (is.data.frame(unit_df) && all(c("VEMF", "UEMF") %in% names(unit_df))) {
  unit_map <- unit_df %>%
    mutate(VEMF = as.character(VEMF), UEMF = as.character(UEMF)) %>%
    distinct(VEMF, .keep_all = TRUE) %>%
    select(VEMF, UEMF)
}

vemf_list <- sort(unique(plot_df$VEMF))
message("Rows loaded: ", nrow(raw_df))
message("Rows after filter: ", nrow(plot_df))
message("Selected scenarios: ", paste(target_scenarios, collapse = ", "))
message("VEMF count: ", length(vemf_list))
message("Output directory: ", run_dir)

written <- 0
for (v in vemf_list) {
  df_v <- plot_df %>%
    filter(VEMF == v) %>%
    arrange(Region, Scenario, YEMF)

  if (nrow(df_v) == 0) {
    next
  }

  df_line <- df_v %>%
    group_by(Region, Scenario) %>%
    filter(n() >= 2) %>%
    ungroup()

  y_label <- args$symbol
  if (!is.null(unit_map)) {
    unit_val <- unit_map$UEMF[unit_map$VEMF == v]
    if (length(unit_val) > 0 && !is.na(unit_val[1]) && nzchar(unit_val[1])) {
      y_label <- paste0(args$symbol, " [", unit_val[1], "]")
    }
  }

  p <- ggplot(df_v, aes(x = YEMF, y = value, color = Scenario)) +
    geom_line(data = df_line, aes(group = Scenario), linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.5, na.rm = TRUE) +
    facet_wrap(~Region, ncol = 6, scales = "free_y") +
    scale_color_manual(values = color_values, drop = FALSE) +
    scale_x_continuous(breaks = sort(unique(df_v$YEMF))) +
    labs(
      title = paste0("Scenario Comparison: ", v),
      subtitle = paste(target_scenarios, collapse = " | "),
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

  out_file <- file.path(run_dir, paste0(safe_file_name(v), ".png"))
  ggsave(
    filename = out_file,
    plot = p,
    width = 18,
    height = 10,
    dpi = 150
  )
  written <- written + 1
}

message("Finished. Files written: ", written)
