library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(gdxrrw)

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
script_dir <- if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()

data_dir   <- file.path(script_dir, "..", "data")
output_dir <- file.path(script_dir, "..", "output", "AFOLUBdata_delta")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

region_list <- c(
  # Asia
  "JPN", "CHN", "IND", "IDN", "KOR", "THA", "VNM",
  # Americas / Europe / Oceania
  "USA", "CAN", "MEX", "BRA", "ARG",
  "DEU", "FRA", "GBR", "ITA", "ESP",
  "AUS", "NZL",
  # Africa
  "ZAF", "EGY", "KEN", "ETH", "TZA", "NGA",
  # Middle East
  "SAU", "IRN", "TUR", "IRL"
)

# Core variables to spot version-to-version changes.
variable_list <- c(
  "Emi_equ_GHG_AFO_Agr",
  "Emi_equ_GHG_AFO_Lan",
  "Emi_Red_equ_GHG_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Lan",
  "Red_Cos_AFO_Agr_CH4",
  "Red_Cos_AFO_Agr_N2O"
)

map_path <- file.path(data_dir, "AFOLUBdata.gdx")
new_path <- file.path(data_dir, "AFOLUBdata.gdx")
old_path <- file.path(data_dir, "AFOLUBdata_0122.gdx")

read_afolub <- function(gdx_path, source_label, map_gdx) {
  vemf_map <- rgdx.set(map_gdx, "VEMF_MAP")

  raw <- rgdx.param(gdx_path, "AFOLUBdata")

  if ("SCE_AFOLUB" %in% names(raw)) {
    raw <- rename(raw, Scenario = SCE_AFOLUB)
  } else if ("sce" %in% names(raw)) {
    raw <- rename(raw, Scenario = sce)
  }

  if ("cty" %in% names(raw)) {
    raw <- rename(raw, Region = cty)
  }

  if ("yr" %in% names(raw)) {
    raw <- rename(raw, Year = yr)
  }

  raw %>%
    rename(Value = AFOLUBdata) %>%
    left_join(
      vemf_map %>% select(VEMF, Var, Item, gas, Tech),
      by = c("Var", "Item", "gas", "Tech")
    ) %>%
    rename(Variable = VEMF) %>%
    select(Scenario, Region, Variable, Year, Value) %>%
    mutate(
      Year = as.numeric(as.character(Year)),
      Region = factor(Region, levels = region_list),
      Source = source_label
    )
}

af_new <- read_afolub(new_path, "new", map_path)
af_old <- read_afolub(old_path, "old", map_path)

af_both <- bind_rows(af_new, af_old) %>%
  filter(
    Scenario == "SSP2_CM_NoCC",
    Region %in% region_list,
    Variable %in% variable_list
  ) %>%
  select(-Scenario)

af_delta <- af_both %>%
  select(Region, Variable, Year, Value, Source) %>%
  pivot_wider(names_from = Source, values_from = Value) %>%
  mutate(
    Delta = new - old,
    DeltaPct = if_else(is.na(old) | old == 0, NA_real_, 100 * (new - old) / abs(old))
  )

for (v in variable_list) {
  df_v <- af_delta %>% filter(Variable == v)

  if (nrow(df_v) == 0) {
    warning("No data for Variable: ", v, " (skip)")
    next
  }

  p <- ggplot(df_v, aes(x = Year, y = Delta)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
    geom_line(color = "#2C7FB8", linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
    labs(
      title = paste0(v, " | delta (new - old)"),
      x = "Year",
      y = "Delta"
    ) +
    theme_classic(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
      strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.4),
      strip.text = element_text(size = 10),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      plot.title = element_text(face = "bold"),
      panel.spacing = unit(0.8, "lines")
    )

  safe_v <- v %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_")

  out_file <- file.path(output_dir, paste0(safe_v, "_delta.png"))
  ggsave(out_file, p, width = 13, height = 8.5, dpi = 220, bg = "white")
  message("Saved: ", out_file)
}
