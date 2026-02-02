library(dplyr)
library(ggplot2)
library(stringr)
library(gdxrrw)

data_dir   <- "../data"
output_dir <- "../output/future"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# region_list <- c("R5REF", "R5OECD90_EU", "R5ASIA", "R5MAF", "R5LAM", "World")
# region_list <- c("BRA","CAN","CHN","IND","JPN","TUR","USA","XE25","XER","XOC","XSE","XSA","XLM","CIS","XME","XNF","XAF","World")
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
  "SAU", "IRN", "TUR","IRL"
)



RR_afolub <- rgdx.param(paste0("../data/AFOLUBdata.gdx"), "RR")  %>% filter(SCE_AFOLUB == "SSP2_CM_NoCC") %>% select(-SCE_AFOLUB)
RR_harmsen <- rgdx.param(paste0("../data/SSP2_500C_NoCC_World.gdx"), "RR_load")


RR_both <- bind_rows(
  RR_afolub %>%
    transmute(
      c=a, src, gas,
      Region = cty,
      Year   = as.numeric(as.character(yr)),
      Value  = RR,
      Source = "AFOLUB"
    ),
  RR_harmsen %>%
    transmute(
      c, src, gas,
      Region = cty,
      Year   = as.numeric(as.character(Y)),
      Value  = RR_load,
      Source = "Harmsen"
    )
) %>%
  filter(Region %in% region_list) %>%
  mutate(Region = factor(Region, levels = region_list))

# ---- 2) ここだけ編集：選びたい候補リスト ----
c_list   <- c("wht","rce","cmt","mlk")  # 例：c("wht","ric",...)
src_list <- c(
  "crop_res_dir",
  "crop_res_ind",
  "crop_res_bun",
  "syn_fert_dir",
  "syn_fert_ind_lch",
  "syn_fert_ind_vol",
  "rce_clt",
  "ent",
  "man_man"
)
gas_list <- c("N2O","CH4")  # 例：c("N2O","CH4",...)

# ---- 3) リストから全組み合わせを生成 → データがあるものだけ残す ----
combo_tbl <- tidyr::expand_grid(
  c = c_list,
  src = src_list,
  gas = gas_list
) %>%
  semi_join(
    RR_both %>% distinct(c, src, gas),
    by = c("c", "src", "gas")
  )

# もし「指定したのに1個もヒットしない」場合に分かるように
if (nrow(combo_tbl) == 0) {
  stop("No matching (c, src, gas) combinations found in RR_both for the given lists.")
}

# ---- 4) comboごとに保存 ----
for (i in seq_len(nrow(combo_tbl))) {
  
  cc <- combo_tbl$c[i]
  ss <- combo_tbl$src[i]
  gg <- combo_tbl$gas[i]
  
  df_i <- RR_both %>% filter(c == cc, src == ss, gas == gg)
  
  # semi_joinしてるので基本0にはならないけど保険
  if (nrow(df_i) == 0) next
  
  title_txt <- paste0("RR | c=", cc, " | src=", ss, " | gas=", gg)
  
  p <- ggplot(df_i, aes(x = Year, y = Value, color = Source, group = Source)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
    labs(
      title = title_txt,
      x = "Year",
      color = "Source"
    ) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "bottom",
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
  
  safe_name <- paste0("RR_c=", cc, "_src=", ss, "_gas=", gg) %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_")
  
  out_file <- file.path(output_dir, paste0("RR/",safe_name, ".png"))
  ggsave(out_file, p, width = 13, height = 8.5, dpi = 220, bg = "white")
  message("Saved: ", out_file)
}




AC_afolub <- rgdx.param(paste0("../data/AFOLUBdata.gdx"), "AC") %>% filter(SCE_AFOLUB == "SSP2_CM_NoCC") %>% select(-SCE_AFOLUB)
AC_harmsen <- rgdx.param(paste0("../data/SSP2_500C_NoCC_World.gdx"), "AC_load")

# ---- 1) 2つを共通フォーマットに揃えて結合 ----
AC_both <- bind_rows(
  AC_afolub %>%
    transmute(
      c      = a,          # a を c に統一
      Region = cty,
      Year   = as.numeric(as.character(yr)),
      Value  = AC,
      Source = "AFOLUB"
    ),
  AC_harmsen %>%
    transmute(
      c      = c,
      Region = cty,
      Year   = as.numeric(as.character(Y)),
      Value  = AC_load,
      Source = "Harmsen"
    )
) %>%
  filter(Region %in% region_list) %>%
  mutate(Region = factor(Region, levels = region_list))

# ---- 2) ここだけ編集：選びたい候補リスト ----
c_list <- c(
  "wht","rce","crl","mze","str","sgr","pls","nut","ocr","vgt",
  "frt","stm","spc","cmt","pmt","omt","rmt","mlk","egg"
)
# ---- 3) リストから対象cだけに絞り、存在するものだけ残す ----
c_tbl <- tibble(c = c_list) %>%
  semi_join(AC_both %>% distinct(c), by = "c")

if (nrow(c_tbl) == 0) {
  stop("No matching c found in AC_both for the given c_list.")
}

# ---- 4) cごとに保存 ----
for (i in seq_len(nrow(c_tbl))) {
  
  cc <- c_tbl$c[i]
  df_i <- AC_both %>% filter(c == cc)
  
  if (nrow(df_i) == 0) next
  
  title_txt <- paste0("AC | c=", cc)
  
  p <- ggplot(df_i, aes(x = Year, y = Value, color = Source, group = Source)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
    labs(
      title = title_txt,
      x = "Year",
      color = "Source"
    ) +
    theme_classic(base_size = 11) +
    theme(
      legend.position = "bottom",
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
  
  safe_name <- paste0("AC_c=", cc) %>%
    str_replace_all("[/\\\\:*?\"<>|]", "_") %>%
    str_replace_all("\\s+", "_")
  
  out_file <- file.path(output_dir, paste0("AC/",safe_name, ".png"))
  ggsave(out_file, p, width = 13, height = 8.5, dpi = 220, bg = "white")
  message("Saved: ", out_file)
}
