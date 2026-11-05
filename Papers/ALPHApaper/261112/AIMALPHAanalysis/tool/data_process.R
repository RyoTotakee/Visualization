library(gdxrrw)

gdx_in  <- "./data/data.gdx"
gdx_out <- "../data/data.gdx" 

keep_syms <- c("Prc_Car", "QH", "Yld", "QL", "QSUP", "PP", "QM")

syminfo_list <- lapply(keep_syms, function(nm) list(name = nm, form = "full"))
sym_data_list <- lapply(syminfo_list, function(si) rgdx(gdx_in, si))

wgdx.lst(gdx_out, sym_data_list)


gdx_in  <- "./data/data_agg.gdx"
gdx_out <- "../data/data_agg.gdx" 

keep_syms <- c("GDP_agg", "Pop_agg")

syminfo_list <- lapply(keep_syms, function(nm) list(name = nm, form = "full"))
sym_data_list <- lapply(syminfo_list, function(si) rgdx(gdx_in, si))

wgdx.lst(gdx_out, sym_data_list)

gdx_in  <- "./data/IAMC_historical.gdx"
gdx_out <- "../data/IAMC_historical.gdx" 

keep_syms <- c("IAMC_Template", "IAMC_Template_LUH2", "IAMC_Template_EDGAR")

syminfo_list <- lapply(keep_syms, function(nm) list(name = nm, form = "full"))
sym_data_list <- lapply(syminfo_list, function(si) rgdx(gdx_in, si))

wgdx.lst(gdx_out, sym_data_list)

