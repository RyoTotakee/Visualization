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

print_help <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript Visualization.R [options]",
      "",
      "Required style (recommended):",
      "  --cases \"folder1=scenario1,folder2=scenario2\"",
      "",
      "Options:",
      "  --cases      Comparison pairs (default: old/new with same scenario)",
      "  --data_dir   Data root directory (default: ../data)",
      "  --gdx_name   GDX file name in each folder (default: IAMC_templatePlus.gdx)",
      "  --symbol     GDX symbol name (default: IAMC_templatePlus)",
      "  --out_dir    Output root directory (default: ../output/old_new_lines)",
      "  --run_name   Output subdirectory name (optional, auto if omitted)",
      "  --help       Show this help",
      "",
      "Example:",
      "  Rscript Visualization.R \\",
      "    --cases \"old=SSP2_500C_CACN_DAC_NoCC,new=SSP2_BaU_NoCC\"",
      sep = "\n"
    )
  )
}

parse_args <- function(args) {
  defaults <- list(
    cases = "old=SSP2_500C_CACN_DAC_NoCC,new=SSP2_500C_CACN_DAC_NoCC",
    data_dir = "../data",
    gdx_name = "IAMC_templatePlus.gdx",
    symbol = "IAMC_templatePlus",
    out_dir = "../output/old_new_lines",
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

    if (key == "--cases") {
      defaults$cases <- val
    } else if (key == "--data_dir") {
      defaults$data_dir <- val
    } else if (key == "--gdx_name") {
      defaults$gdx_name <- val
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

safe_file_name <- function(x) {
  y <- gsub("[^A-Za-z0-9._-]", "_", x)
  y <- gsub("_+", "_", y)
  y <- sub("^_", "", y)
  y <- sub("_$", "", y)
  ifelse(nchar(y) == 0, "unknown", y)
}

trim_ws <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

parse_cases <- function(case_text) {
  tokens <- unlist(strsplit(case_text, ",", fixed = TRUE))
  tokens <- trim_ws(tokens)
  tokens <- tokens[nchar(tokens) > 0]
  if (length(tokens) < 2) {
    stop("`--cases` must contain at least two pairs: folder=scenario", call. = FALSE)
  }

  out <- lapply(tokens, function(tok) {
    parts <- unlist(strsplit(tok, "=", fixed = TRUE))
    if (length(parts) != 2) {
      stop(sprintf("Invalid case token: %s (expected folder=scenario)", tok), call. = FALSE)
    }
    folder <- trim_ws(parts[1])
    scenario <- trim_ws(parts[2])
    if (folder == "" || scenario == "") {
      stop(sprintf("Invalid case token: %s (empty folder/scenario)", tok), call. = FALSE)
    }
    data.frame(dataset = folder, scenario = scenario, stringsAsFactors = FALSE)
  })

  case_df <- bind_rows(out)
  if (any(duplicated(case_df$dataset))) {
    stop("Duplicate folder name in `--cases`. Use unique folder names.", call. = FALSE)
  }
  case_df
}

load_one_case <- function(dataset_name, scenario_name, gdx_path, symbol_name) {
  if (!file.exists(gdx_path)) {
    stop(sprintf("GDX file not found: %s", gdx_path), call. = FALSE)
  }

  df <- gdxrrw::rgdx.param(gdx_path, symbol_name)
  required_cols <- c("Scenario", "REMF", "VEMF", "YEMF", symbol_name)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "Missing required columns in %s: %s",
        gdx_path,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  available_scenarios <- sort(unique(as.character(df$Scenario)))
  if (!(scenario_name %in% available_scenarios)) {
    stop(
      sprintf(
        "Scenario '%s' not found in '%s'. Available: %s",
        scenario_name,
        dataset_name,
        paste(available_scenarios, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df %>%
    mutate(
      Scenario = as.character(Scenario),
      REMF = as.character(REMF),
      VEMF = as.character(VEMF),
      YEMF = suppressWarnings(as.integer(as.character(YEMF)))
    ) %>%
    filter(Scenario == scenario_name) %>%
    mutate(dataset = dataset_name)
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

case_df <- parse_cases(args$cases)
data_dir <- normalizePath(file.path(script_dir, args$data_dir), winslash = "/", mustWork = FALSE)
out_base_dir <- normalizePath(file.path(script_dir, args$out_dir), winslash = "/", mustWork = FALSE)

message("Initializing gdxrrw...")
gdxrrw::igdx(silent = TRUE)

message("Loading datasets...")
all_list <- vector("list", nrow(case_df))
for (i in seq_len(nrow(case_df))) {
  dataset_name <- case_df$dataset[i]
  scenario_name <- case_df$scenario[i]
  gdx_path <- file.path(data_dir, dataset_name, args$gdx_name)
  message("  - ", dataset_name, " (scenario: ", scenario_name, ")")
  all_list[[i]] <- load_one_case(
    dataset_name = dataset_name,
    scenario_name = scenario_name,
    gdx_path = gdx_path,
    symbol_name = args$symbol
  )
}

all_df <- bind_rows(all_list)
value_col <- args$symbol

plot_df <- all_df %>%
  filter(REMF %in% region_levels) %>%
  rename(value = all_of(value_col)) %>%
  mutate(
    value = as.numeric(value),
    Region = factor(REMF, levels = region_levels),
    dataset = factor(dataset, levels = case_df$dataset)
  ) %>%
  filter(!is.na(YEMF), !is.na(value), !is.na(VEMF), !is.na(Region))

if (nrow(plot_df) == 0) {
  stop("No data after filtering by selected scenarios and target regions.", call. = FALSE)
}

default_run_name <- paste(paste(case_df$dataset, case_df$scenario, sep = "="), collapse = "__")
run_name <- if (is.null(args$run_name) || args$run_name == "") {
  safe_file_name(default_run_name)
} else {
  safe_file_name(args$run_name)
}
run_dir <- file.path(out_base_dir, run_name)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

datasets <- levels(plot_df$dataset)
color_values <- grDevices::hcl.colors(length(datasets), palette = "Dark 3")
names(color_values) <- datasets
if (all(c("old", "new") %in% tolower(datasets))) {
  old_name <- datasets[tolower(datasets) == "old"][1]
  new_name <- datasets[tolower(datasets) == "new"][1]
  color_values[old_name] <- "#1f77b4"
  color_values[new_name] <- "#d62728"
}

vemf_list <- sort(unique(plot_df$VEMF))
message("Rows loaded: ", nrow(all_df))
message("Rows after region filter: ", nrow(plot_df))
message("Dataset count: ", length(datasets))
message("VEMF count: ", length(vemf_list))
message("Output directory: ", run_dir)

written <- 0
for (v in vemf_list) {
  df_v <- plot_df %>%
    filter(VEMF == v) %>%
    arrange(Region, dataset, YEMF)

  if (nrow(df_v) == 0) {
    next
  }

  df_line <- df_v %>%
    group_by(Region, dataset) %>%
    filter(n() >= 2) %>%
    ungroup()

  subtitle_text <- paste(paste(case_df$dataset, case_df$scenario, sep = "="), collapse = " | ")
  p <- ggplot(df_v, aes(x = YEMF, y = value, color = dataset)) +
    geom_line(data = df_line, aes(group = dataset), linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 1.5, na.rm = TRUE) +
    facet_wrap(~Region, ncol = 6, scales = "free_y") +
    scale_color_manual(values = color_values, drop = FALSE) +
    scale_x_continuous(breaks = sort(unique(df_v$YEMF))) +
    labs(
      title = paste0("Comparison: ", v),
      subtitle = subtitle_text,
      x = "Year",
      y = args$symbol,
      color = "Folder"
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
