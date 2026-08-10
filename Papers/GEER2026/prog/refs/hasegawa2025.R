# 2nd, May, 2025
# Tomoko Hasegawa
# This program is for analysis in the paper titled "National land-based climate mitigation pathways toward long-term climate goals"
install.packages("ncdf4")
install.packages("gdxrrw")
install.packages("rlang")
install.packages("ggplot2", dependencies = TRUE)
library(ncdf4)
library(gdxrrw)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(maps)
library(grid)
library(RColorBrewer)
library(scales)
library(cowplot)
library(car)
library(data.table)
library(tidyr)
library(plyr)

pastelpal1 <- brewer.pal(12, "Pastel1")
pastelpal1 <- brewer.pal(11, "Spectral")
spectpal <- brewer.pal(11, "Spectral")


MyThemeLine_grid <- theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
    panel.grid.minor = element_line(color = "cornsilk2"), 
    panel.grid.major = element_line(color = "cornsilk2"), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=15, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(angle=90, vjust=0.5, hjust=0.5),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7)
    
  )

MyThemeLine_grid2 <- MyThemeLine_grid +
  theme(
    strip.text.x = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

MyThemeLine_grid2h <- MyThemeLine_grid2 +
  theme(
    axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5)
  )

MyThemeLine_grid3 <- theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
#    panel.grid.minor = element_line(color = NA), 
#    panel.grid.major=element_blank(),
    panel.grid.minor = element_line(color = "cornsilk2"), 
    panel.grid.major = element_line(color = "cornsilk2"), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(angle=0, vjust=0.5, hjust=0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size =10)
    #    legend.background = element_rect(fill="lightgray"))
    
  )

#-----Labels
fvarname1 <- function(y){
  
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_GHG_AFO_Agr"] <- "Agriculture-Emis"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_GHG_AFO_Pos_Emi_Lan"] <- "Land-Emis"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_GHG_AFO_Neg_Emi"] <- "Land-Sink 2"
  
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_GHG_AFO"] <- "Emissions - Agr + LUC"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_CH4_AFO_Agr"] <- "Agriculture - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_N2O_AFO_Agr"] <- "Agriculture - N2O"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_AFO_Lan"] <- "Land - CO2"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For"] <- "Land-Sink"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har"] <- "Emissions from wood harvest"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"] <- "Forest fire - CO2"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_AFO_Lan_For_Bur"] <- "Emissions from forest burning"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff"] <- "Sink through afforestation"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha"] <- "Land conversion - CO2"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_Lan_Use_Flo_RemForest"] <- "Remaining managed forest - CO2"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer"] <- "Enteric Fermentation - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_CH4_AFO_Agr_Liv_Man_Man"] <- "Manure Management - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_CH4_AFO_Agr_Ric"] <- "Rice cultivation - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_N2O_AFO_Agr_Liv_Ent_Fer"] <- "Enteric Fermentation - N2O"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_N2O_AFO_Agr_Liv_Man_Man"] <- "Manure Management - N2O"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_N2O_AFO_Agr_Man_Soi"] <- "Managed Soils - N2O"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_GHG_AFO"] <- "Reduction - Agr + LUC"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer"] <- "Enteric Fermentation - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man"] <- "Manure Management - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Ric"] <- "Rice cultivation - CH4"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Liv_Ent_Fer"] <- "Enteric Fermentation - N2O"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man"] <- "Manure Management - N2O"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi"] <- "Managed Soils - N2O"

  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre"] <- "Enteric Fermentation - Breeding management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee"] <- "Enteric Fermentation - Feed management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad"] <- "Manure Management - Covered anaerobic digesters"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And"] <- "Manure Management - Anaerobic digestion"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl"] <- "Manure Management - Dome digester and replacement by generated gas"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc"] <- "Manure Management - Anaerobic decomposition"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams"] <- "Manure Management - Replace urea with ammonium sulfate"
  
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Ric_Wat"] <- "Rice - Water management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_CH4_AFO_Agr_Ric_Res"] <- "Rice - Residue management"
  
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til"] <- "Managed Soils -Tillage management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer"] <- "Managed Soils -Fertilization management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res"] <- "Managed Soils -Residue manage"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Pla"] <- "Managed Soils -Plant management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Bio"] <- "Managed Soils -Bioenergy"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo"] <- "Managed Soils -Slow-release fertilizer"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin"] <- "Managed Soils -Nitrification Inhibitors"

  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_equ_GHG_AFO_Agr"] <- "Agr - total"  
  
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Pla"] <- "Plantation"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Ref"] <- "Reforestation"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_FPr"] <- "Forest protection (measure)"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Pff"] <- "Prevention of forest fire"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Ril"] <- "Reduced Impact logging"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Enr"] <- "Enhanced natural regeneration"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Agf"] <- "Agroforestry"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Wmp"] <- "Water management on peatland"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Prr"] <- "Peat restoration"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Agr_Seq"] <- "Agriculture - Carbon sequestration"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Pro"] <- "Forest protection"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Res"] <- "Forest restoration"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Man"] <- "Forest management"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Bch"] <- "Biochar"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Scs"] <- "Soil Carbon Sequestration"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Scc"] <- "SCS cropland"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_CO2_AFO_Lan_Scg"] <- "SCS grassland"
  levels(y$Variable2)[levels(y$Variable2)=="Car_Seq_Lan_Use_Soi_Car_Man"] <- "Soil Carbon Sequestration"
  levels(y$Variable2)[levels(y$Variable2)=="Car_Seq_Lan_Use_Soi_Car_Man_Cro"] <- "SCS cropland (PLUM)"
  levels(y$Variable2)[levels(y$Variable2)=="Car_Seq_Lan_Use_Soi_Car_Man_Gra"] <- "SCS grassland (PLUM)"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_CO2_AFO_Aff"] <- "Afforestation"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_GHG_AFO_Seq"] <- "Land - Sink"
  
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Agr_Liv_Ent_Fer"] <- "Agriculture - Enteric Fermentation"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Agr_Liv_Man_Man"] <- "Agriculture - Manure Management"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Agr_Man_Soi"] <- "Agriculture - Managed Soils"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Agr_Ric"] <- "Agriculture - Rice"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan"] <- "Land - total"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Pla"] <- "Land - Plantation"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Ref"] <- "Land - Reforestation"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_FPr"] <- "Land - Forest protection"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Pff"] <- "Land - Prevention of forest fire"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Ril"] <- "Land - Reduced Impact logging"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Enr"] <- "Land - Enhanced natural regeneration"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Agf"] <- "Land - Agroforestry"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Wmp"] <- "Land - Water management on peatland"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Prr"] <- "Land - Peat restoration"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Bch"] <- "Land - Biochar"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Scc"] <- "Land - SCS cropland"
  levels(y$Variable2)[levels(y$Variable2)=="Red_Cos_AFO_Lan_Scg"] <- "Land - SCS grassland"
  
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are"] <- "Land Area"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Pla"] <- "Reforestation"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Ref"] <- "Forest protection"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_FPr"] <- "Prevention of forest fire"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Pff"] <- "Other"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Ril"] <- "Reduced Impact logging"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Enr"] <- "Enhanced natural regeneration"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Agf"] <- "Agroforestry"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Wmp"] <- "Water management on peatland"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Prr"] <- "Peat restoration"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Bch"] <- "Biochar"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Scc"] <- "SCS cropland"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Are_Scg"] <- "SCS grassland"
  
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_Rat_GHG_AFO_Agr"] <- "Reduction ratio (GHG)"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_Rat_CH4_AFO_Agr"] <- "Reduction ratio (CH4)"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_Rat_N2O_AFO_Agr"] <- "Reduction ratio (N2O)"
  levels(y$Variable2)[levels(y$Variable2)=="Mar_Cos_GHG_AFO_Agr"] <- "Margical Cost - Agr"
  levels(y$Variable2)[levels(y$Variable2)=="Mar_Cos_GHG_AFO_Lan"] <- "Margical Cost - Land"
  levels(y$Variable2)[levels(y$Variable2)=="Mar_Cos_GHG_AFO"] <- "Margical Cost - AFOLU"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_Red_GHG_AFO_Die_Cha"] <- "Dietary change"
  
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Cov_Frs_Unm"] <- "Unmanaged forest"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Cov_Frs_Man"] <- "Managed forest"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Cov_Pst"] <- "Pasture"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Cov_Oth_Lan"] <- "Other natural land"
  levels(y$Variable2)[levels(y$Variable2)=="Lan_Cov_Cro"] <- "Cropland"
  

  z <- y
  return(z)
}

fvarname2 <- function(y){
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_GHG_AFO"] <- "Baseline"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_GHG_AFO_Residual"] <- "Climate policy"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_GHG_AFO_TRS"] <- "Baseline after translation"
  levels(y$Variable2)[levels(y$Variable2)=="Emi_equ_GHG_AFO_Residual_TRS"] <- "Climate policy after translation"
  z <- y
  return(z)
}
fregname1 <- function(y){
  levels(y$Region)[levels(y$Region)=="R5OECD90+EU"] <- "OECD and EU"
  levels(y$Region)[levels(y$Region)=="R5REF"] <- "Reforming economies"
  levels(y$Region)[levels(y$Region)=="R5ASIA"] <- "Asia"
  levels(y$Region)[levels(y$Region)=="R5MAF"] <- "Middle East and Africa"
  levels(y$Region)[levels(y$Region)=="R5LAM"] <- "Latin America"
  
  z <- y
  return(z)
}

fregname2 <- function(y){

  levels(y$Region)[levels(y$Region)=="AFG"] <- "Afghanistan"
  levels(y$Region)[levels(y$Region)=="ALB"] <- "Albania"
  levels(y$Region)[levels(y$Region)=="DZA"] <- "Algeria"
  levels(y$Region)[levels(y$Region)=="ASM"] <- "American Samoa"
  levels(y$Region)[levels(y$Region)=="AND"] <- "Andorra"
  levels(y$Region)[levels(y$Region)=="AGO"] <- "Angola"
  levels(y$Region)[levels(y$Region)=="ATG"] <- "Antigua and Barbuda"
  levels(y$Region)[levels(y$Region)=="ARG"] <- "Argentina"
  levels(y$Region)[levels(y$Region)=="ARM"] <- "Armenia"
  levels(y$Region)[levels(y$Region)=="ABW"] <- "Aruba"
  levels(y$Region)[levels(y$Region)=="AUS"] <- "Australia"
  levels(y$Region)[levels(y$Region)=="AUT"] <- "Austria"
  levels(y$Region)[levels(y$Region)=="AZE"] <- "Azerbaijan"
  levels(y$Region)[levels(y$Region)=="BHS"] <- "Bahamas"
  levels(y$Region)[levels(y$Region)=="BHR"] <- "Bahrain"
  levels(y$Region)[levels(y$Region)=="BGD"] <- "Bangladesh"
  levels(y$Region)[levels(y$Region)=="BRB"] <- "Barbados"
  levels(y$Region)[levels(y$Region)=="BLR"] <- "Belarus"
  levels(y$Region)[levels(y$Region)=="BEL"] <- "Belgium"
  levels(y$Region)[levels(y$Region)=="BLZ"] <- "Belize"
  levels(y$Region)[levels(y$Region)=="BEN"] <- "Benin"
  levels(y$Region)[levels(y$Region)=="BMU"] <- "Bermuda"
  levels(y$Region)[levels(y$Region)=="BTN"] <- "Bhutan"
  levels(y$Region)[levels(y$Region)=="BOL"] <- "Bolivia"
  levels(y$Region)[levels(y$Region)=="BIH"] <- "Bosnia and Herzegovina"
  levels(y$Region)[levels(y$Region)=="BWA"] <- "Botswana"
  levels(y$Region)[levels(y$Region)=="BRA"] <- "Brazil"
  levels(y$Region)[levels(y$Region)=="VGB"] <- "British Virgin Islands"
  levels(y$Region)[levels(y$Region)=="BRN"] <- "Brunei Darussalam"
  levels(y$Region)[levels(y$Region)=="BGR"] <- "Bulgaria"
  levels(y$Region)[levels(y$Region)=="BFA"] <- "Burkina\nFaso"
  levels(y$Region)[levels(y$Region)=="BDI"] <- "Burundi"
  levels(y$Region)[levels(y$Region)=="CPV"] <- "Cabo Verde"
  levels(y$Region)[levels(y$Region)=="KHM"] <- "Cambodia"
  levels(y$Region)[levels(y$Region)=="CMR"] <- "Cameroon"
  levels(y$Region)[levels(y$Region)=="CAN"] <- "Canada"
  levels(y$Region)[levels(y$Region)=="CYM"] <- "Cayman Islands"
  levels(y$Region)[levels(y$Region)=="CAF"] <- "Central\nAfrican Rep."
  levels(y$Region)[levels(y$Region)=="TCD"] <- "Chad"
  levels(y$Region)[levels(y$Region)=="CHI"] <- "Channel Islands"
  levels(y$Region)[levels(y$Region)=="CHL"] <- "Chile"
  levels(y$Region)[levels(y$Region)=="CHN"] <- "China"
  levels(y$Region)[levels(y$Region)=="COL"] <- "Colombia"
  levels(y$Region)[levels(y$Region)=="COM"] <- "Comoros"
  levels(y$Region)[levels(y$Region)=="COD"] <- "Congo D.R."
  levels(y$Region)[levels(y$Region)=="COG"] <- "Congo, Rep."
  levels(y$Region)[levels(y$Region)=="COK"] <- "Cook Islands"
  levels(y$Region)[levels(y$Region)=="CRI"] <- "Costa Rica"
  levels(y$Region)[levels(y$Region)=="CIV"] <- "Cote\nd'Ivoire"
  levels(y$Region)[levels(y$Region)=="HRV"] <- "Croatia"
  levels(y$Region)[levels(y$Region)=="CUB"] <- "Cuba"
  levels(y$Region)[levels(y$Region)=="CUW"] <- "Curacao"
  levels(y$Region)[levels(y$Region)=="CYP"] <- "Cyprus"
  levels(y$Region)[levels(y$Region)=="CZE"] <- "Czech Republic"
  levels(y$Region)[levels(y$Region)=="DNK"] <- "Denmark"
  levels(y$Region)[levels(y$Region)=="DJI"] <- "Djibouti"
  levels(y$Region)[levels(y$Region)=="DMA"] <- "Dominica"
  levels(y$Region)[levels(y$Region)=="DOM"] <- "Dominican\nRep."
  levels(y$Region)[levels(y$Region)=="ECU"] <- "Ecuador"
  levels(y$Region)[levels(y$Region)=="EGY"] <- "Egypt"
  levels(y$Region)[levels(y$Region)=="SLV"] <- "El Salvador"
  levels(y$Region)[levels(y$Region)=="GNQ"] <- "Equatorial Guinea"
  levels(y$Region)[levels(y$Region)=="ERI"] <- "Eritrea"
  levels(y$Region)[levels(y$Region)=="EST"] <- "Estonia"
  levels(y$Region)[levels(y$Region)=="SWZ"] <- "Eswatini"
  levels(y$Region)[levels(y$Region)=="ETH"] <- "Ethiopia"
  levels(y$Region)[levels(y$Region)=="FRO"] <- "Faroe Islands"
  levels(y$Region)[levels(y$Region)=="FLK"] <- "Falkland Islands (Malvinas)"
  levels(y$Region)[levels(y$Region)=="FJI"] <- "Fiji"
  levels(y$Region)[levels(y$Region)=="FIN"] <- "Finland"
  levels(y$Region)[levels(y$Region)=="FRA"] <- "France"
  levels(y$Region)[levels(y$Region)=="GUF"] <- "French Guyana"
  levels(y$Region)[levels(y$Region)=="PYF"] <- "French Polynesia"
  levels(y$Region)[levels(y$Region)=="GAB"] <- "Gabon"
  levels(y$Region)[levels(y$Region)=="GMB"] <- "Gambia"
  levels(y$Region)[levels(y$Region)=="GEO"] <- "Georgia"
  levels(y$Region)[levels(y$Region)=="DEU"] <- "Germany"
  levels(y$Region)[levels(y$Region)=="GHA"] <- "Ghana"
  levels(y$Region)[levels(y$Region)=="GIB"] <- "Gibraltar"
  levels(y$Region)[levels(y$Region)=="GRC"] <- "Greece"
  levels(y$Region)[levels(y$Region)=="GRL"] <- "Greenland"
  levels(y$Region)[levels(y$Region)=="GRD"] <- "Grenada"
  levels(y$Region)[levels(y$Region)=="GLP"] <- "Guadeloupe"
  levels(y$Region)[levels(y$Region)=="GUM"] <- "Guam"
  levels(y$Region)[levels(y$Region)=="GTM"] <- "Guatemala"
  levels(y$Region)[levels(y$Region)=="GIN"] <- "Guinea"
  levels(y$Region)[levels(y$Region)=="GNB"] <- "Guinea-Bissau"
  levels(y$Region)[levels(y$Region)=="GUY"] <- "Guyana"
  levels(y$Region)[levels(y$Region)=="HTI"] <- "Haiti"
  levels(y$Region)[levels(y$Region)=="HND"] <- "Honduras"
  levels(y$Region)[levels(y$Region)=="HKG"] <- "Hong Kong SAR, China"
  levels(y$Region)[levels(y$Region)=="HUN"] <- "Hungary"
  levels(y$Region)[levels(y$Region)=="ISL"] <- "Iceland"
  levels(y$Region)[levels(y$Region)=="IND"] <- "India"
  levels(y$Region)[levels(y$Region)=="IDN"] <- "Indonesia"
  levels(y$Region)[levels(y$Region)=="IRN"] <- "Iran"
  levels(y$Region)[levels(y$Region)=="IRQ"] <- "Iraq"
  levels(y$Region)[levels(y$Region)=="IRL"] <- "Ireland"
  levels(y$Region)[levels(y$Region)=="IMN"] <- "Isle of Man"
  levels(y$Region)[levels(y$Region)=="ISR"] <- "Israel"
  levels(y$Region)[levels(y$Region)=="ITA"] <- "Italy"
  levels(y$Region)[levels(y$Region)=="JAM"] <- "Jamaica"
  levels(y$Region)[levels(y$Region)=="JPN"] <- "Japan"
  levels(y$Region)[levels(y$Region)=="JOR"] <- "Jordan"
  levels(y$Region)[levels(y$Region)=="KAZ"] <- "Kazakhstan"
  levels(y$Region)[levels(y$Region)=="KEN"] <- "Kenya"
  levels(y$Region)[levels(y$Region)=="KIR"] <- "Kiribati"
  levels(y$Region)[levels(y$Region)=="PRK"] <- "Korea, DPR."
  levels(y$Region)[levels(y$Region)=="KOR"] <- "South\nKorea"
  levels(y$Region)[levels(y$Region)=="XKX"] <- "Kosovo"
  levels(y$Region)[levels(y$Region)=="KWT"] <- "Kuwait"
  levels(y$Region)[levels(y$Region)=="KGZ"] <- "Kyrgyz Republic"
  levels(y$Region)[levels(y$Region)=="LAO"] <- "Lao PDR"
  levels(y$Region)[levels(y$Region)=="LVA"] <- "Latvia"
  levels(y$Region)[levels(y$Region)=="LBN"] <- "Lebanon"
  levels(y$Region)[levels(y$Region)=="LSO"] <- "Lesotho"
  levels(y$Region)[levels(y$Region)=="LBR"] <- "Liberia"
  levels(y$Region)[levels(y$Region)=="LBY"] <- "Libya"
  levels(y$Region)[levels(y$Region)=="LIE"] <- "Liechtenstein"
  levels(y$Region)[levels(y$Region)=="LTU"] <- "Lithuania"
  levels(y$Region)[levels(y$Region)=="LUX"] <- "Luxembourg"
  levels(y$Region)[levels(y$Region)=="MAC"] <- "Macao SAR, China"
  levels(y$Region)[levels(y$Region)=="MDG"] <- "Madagascar"
  levels(y$Region)[levels(y$Region)=="MWI"] <- "Malawi"
  levels(y$Region)[levels(y$Region)=="MYS"] <- "Malaysia"
  levels(y$Region)[levels(y$Region)=="MDV"] <- "Maldives"
  levels(y$Region)[levels(y$Region)=="MLI"] <- "Mali"
  levels(y$Region)[levels(y$Region)=="MLT"] <- "Malta"
  levels(y$Region)[levels(y$Region)=="MHL"] <- "Marshall Islands"
  levels(y$Region)[levels(y$Region)=="MRT"] <- "Mauritania"
  levels(y$Region)[levels(y$Region)=="MUS"] <- "Mauritius"
  levels(y$Region)[levels(y$Region)=="MEX"] <- "Mexico"
  levels(y$Region)[levels(y$Region)=="FSM"] <- "Micronesia, Fed. Sts."
  levels(y$Region)[levels(y$Region)=="MDA"] <- "Moldova"
  levels(y$Region)[levels(y$Region)=="MCO"] <- "Monaco"
  levels(y$Region)[levels(y$Region)=="MNG"] <- "Mongolia"
  levels(y$Region)[levels(y$Region)=="MNE"] <- "Montenegro"
  levels(y$Region)[levels(y$Region)=="MAR"] <- "Morocco"
  levels(y$Region)[levels(y$Region)=="MOZ"] <- "Mozambique"
  levels(y$Region)[levels(y$Region)=="MMR"] <- "Myanmar"
  levels(y$Region)[levels(y$Region)=="NAM"] <- "Namibia"
  levels(y$Region)[levels(y$Region)=="NRU"] <- "Nauru"
  levels(y$Region)[levels(y$Region)=="NPL"] <- "Nepal"
  levels(y$Region)[levels(y$Region)=="NLD"] <- "Netherlands"
  levels(y$Region)[levels(y$Region)=="NCL"] <- "New Caledonia"
  levels(y$Region)[levels(y$Region)=="NZL"] <- "New Zealand"
  levels(y$Region)[levels(y$Region)=="NIC"] <- "Nicaragua"
  levels(y$Region)[levels(y$Region)=="NER"] <- "Niger"
  levels(y$Region)[levels(y$Region)=="NGA"] <- "Nigeria"
  levels(y$Region)[levels(y$Region)=="MKD"] <- "North Macedonia"
  levels(y$Region)[levels(y$Region)=="MNP"] <- "Northern Mariana Islands"
  levels(y$Region)[levels(y$Region)=="NOR"] <- "Norway"
  levels(y$Region)[levels(y$Region)=="OMN"] <- "Oman"
  levels(y$Region)[levels(y$Region)=="PAK"] <- "Pakistan"
  levels(y$Region)[levels(y$Region)=="PLW"] <- "Palau"
  levels(y$Region)[levels(y$Region)=="PAN"] <- "Panama"
  levels(y$Region)[levels(y$Region)=="PNG"] <- "Papua New Guinea"
  levels(y$Region)[levels(y$Region)=="PRY"] <- "Paraguay"
  levels(y$Region)[levels(y$Region)=="PER"] <- "Peru"
  levels(y$Region)[levels(y$Region)=="PHL"] <- "Philippines"
  levels(y$Region)[levels(y$Region)=="POL"] <- "Poland"
  levels(y$Region)[levels(y$Region)=="PRT"] <- "Portugal"
  levels(y$Region)[levels(y$Region)=="PRI"] <- "Puerto Rico"
  levels(y$Region)[levels(y$Region)=="QAT"] <- "Qatar"
  levels(y$Region)[levels(y$Region)=="ROU"] <- "Romania"
  levels(y$Region)[levels(y$Region)=="RUS"] <- "Russia"
  levels(y$Region)[levels(y$Region)=="RWA"] <- "Rwanda"
  levels(y$Region)[levels(y$Region)=="WSM"] <- "Samoa"
  levels(y$Region)[levels(y$Region)=="SMR"] <- "San Marino"
  levels(y$Region)[levels(y$Region)=="STP"] <- "Sao Tome and Principe"
  levels(y$Region)[levels(y$Region)=="SAU"] <- "Saudi Arabia"
  levels(y$Region)[levels(y$Region)=="SEN"] <- "Senegal"
  levels(y$Region)[levels(y$Region)=="SRB"] <- "Serbia"
  levels(y$Region)[levels(y$Region)=="SYC"] <- "Seychelles"
  levels(y$Region)[levels(y$Region)=="SLE"] <- "Sierra Leone"
  levels(y$Region)[levels(y$Region)=="SGP"] <- "Singapore"
  levels(y$Region)[levels(y$Region)=="SXM"] <- "Sint Maarten (Dutch part)"
  levels(y$Region)[levels(y$Region)=="SVK"] <- "Slovak Republic"
  levels(y$Region)[levels(y$Region)=="SVN"] <- "Slovenia"
  levels(y$Region)[levels(y$Region)=="SLB"] <- "Solomon Islands"
  levels(y$Region)[levels(y$Region)=="SOM"] <- "Somalia"
  levels(y$Region)[levels(y$Region)=="ZAF"] <- "South\nAfrica"
  levels(y$Region)[levels(y$Region)=="SSD"] <- "South Sudan"
  levels(y$Region)[levels(y$Region)=="ESP"] <- "Spain"
  levels(y$Region)[levels(y$Region)=="LKA"] <- "Sri Lanka"
  levels(y$Region)[levels(y$Region)=="KNA"] <- "St. Kitts and Nevis"
  levels(y$Region)[levels(y$Region)=="LCA"] <- "St. Lucia"
  levels(y$Region)[levels(y$Region)=="MAF"] <- "St. Martin (French part)"
  levels(y$Region)[levels(y$Region)=="VCT"] <- "St. Vincent and the Grenadines"
  levels(y$Region)[levels(y$Region)=="SDN"] <- "Sudan"
  levels(y$Region)[levels(y$Region)=="SUR"] <- "Suriname"
  levels(y$Region)[levels(y$Region)=="SWE"] <- "Sweden"
  levels(y$Region)[levels(y$Region)=="CHE"] <- "Switzerland"
  levels(y$Region)[levels(y$Region)=="SYR"] <- "Syria"
  levels(y$Region)[levels(y$Region)=="TJK"] <- "Tajikistan"
  levels(y$Region)[levels(y$Region)=="TZA"] <- "Tanzania"
  levels(y$Region)[levels(y$Region)=="THA"] <- "Thailand"
  levels(y$Region)[levels(y$Region)=="TLS"] <- "Timor-Leste"
  levels(y$Region)[levels(y$Region)=="TGO"] <- "Togo"
  levels(y$Region)[levels(y$Region)=="TON"] <- "Tonga"
  levels(y$Region)[levels(y$Region)=="TTO"] <- "Trinidad and Tobago"
  levels(y$Region)[levels(y$Region)=="TUN"] <- "Tunisia"
  levels(y$Region)[levels(y$Region)=="TUR"] <- "Turkey"
  levels(y$Region)[levels(y$Region)=="TKM"] <- "Turkmenistan"
  levels(y$Region)[levels(y$Region)=="TCA"] <- "Turks and Caicos Islands"
  levels(y$Region)[levels(y$Region)=="TUV"] <- "Tuvalu"
  levels(y$Region)[levels(y$Region)=="UGA"] <- "Uganda"
  levels(y$Region)[levels(y$Region)=="UKR"] <- "Ukraine"
  levels(y$Region)[levels(y$Region)=="ARE"] <- "United Arab Emirates"
  levels(y$Region)[levels(y$Region)=="GBR"] <- "United\nKingdom"
  levels(y$Region)[levels(y$Region)=="USA"] <- "United\nStates"
  levels(y$Region)[levels(y$Region)=="URY"] <- "Uruguay"
  levels(y$Region)[levels(y$Region)=="UZB"] <- "Uzbekistan"
  levels(y$Region)[levels(y$Region)=="VUT"] <- "Vanuatu"
  levels(y$Region)[levels(y$Region)=="VEN"] <- "Venezuela"
  levels(y$Region)[levels(y$Region)=="VNM"] <- "Vietnam"
  levels(y$Region)[levels(y$Region)=="VIR"] <- "Virgin Islands (U.S.)"
  levels(y$Region)[levels(y$Region)=="PSE"] <- "West Bank and Gaza"
  levels(y$Region)[levels(y$Region)=="YEM"] <- "Yemen"
  levels(y$Region)[levels(y$Region)=="ZMB"] <- "Zambia"
  levels(y$Region)[levels(y$Region)=="ZWE"] <- "Zimbabwe"
  levels(y$Region)[levels(y$Region)=="ARB"] <- "Arab World"
  levels(y$Region)[levels(y$Region)=="CSS"] <- "Caribbean small states"
  levels(y$Region)[levels(y$Region)=="CEB"] <- "Central Europe and the Baltics"
  levels(y$Region)[levels(y$Region)=="EAR"] <- "Early-demographic dividend"
  levels(y$Region)[levels(y$Region)=="EAS"] <- "East Asia & Pacific"
  levels(y$Region)[levels(y$Region)=="EAP"] <- "East Asia & Pacific (excluding high income)"
  levels(y$Region)[levels(y$Region)=="TEA"] <- "East Asia & Pacific (IDA & IBRD countries)"
  levels(y$Region)[levels(y$Region)=="EMU"] <- "Euro area"
  levels(y$Region)[levels(y$Region)=="ECS"] <- "Europe & Central Asia"
  levels(y$Region)[levels(y$Region)=="ECA"] <- "Europe & Central Asia (excluding high income)"
  levels(y$Region)[levels(y$Region)=="TEC"] <- "Europe & Central Asia (IDA & IBRD countries)"
  levels(y$Region)[levels(y$Region)=="EUU"] <- "European Union"
  levels(y$Region)[levels(y$Region)=="FCS"] <- "Fragile and conflict affected situations"
  levels(y$Region)[levels(y$Region)=="HPC"] <- "Heavily indebted poor countries (HIPC)"
  levels(y$Region)[levels(y$Region)=="HIC"] <- "High income"
  levels(y$Region)[levels(y$Region)=="IBD"] <- "IBRD only"
  levels(y$Region)[levels(y$Region)=="IBT"] <- "IDA & IBRD total"
  levels(y$Region)[levels(y$Region)=="IDB"] <- "IDA blend"
  levels(y$Region)[levels(y$Region)=="IDX"] <- "IDA only"
  levels(y$Region)[levels(y$Region)=="IDA"] <- "IDA total"
  levels(y$Region)[levels(y$Region)=="LTE"] <- "Late-demographic dividend"
  levels(y$Region)[levels(y$Region)=="LCN"] <- "Latin America & Caribbean"
  levels(y$Region)[levels(y$Region)=="LAC"] <- "Latin America & Caribbean (excluding high income)"
  levels(y$Region)[levels(y$Region)=="TLA"] <- "Latin America & the Caribbean (IDA & IBRD countries)"
  levels(y$Region)[levels(y$Region)=="LDC"] <- "Least developed countries: UN classification"
  levels(y$Region)[levels(y$Region)=="LMY"] <- "Low & middle income"
  levels(y$Region)[levels(y$Region)=="LIC"] <- "Low income"
  levels(y$Region)[levels(y$Region)=="LMC"] <- "Lower middle income"
  levels(y$Region)[levels(y$Region)=="MEA"] <- "Middle East & North Africa"
  levels(y$Region)[levels(y$Region)=="MNA"] <- "Middle East & North Africa (excluding high income)"
  levels(y$Region)[levels(y$Region)=="TMN"] <- "Middle East & North Africa (IDA & IBRD countries)"
  levels(y$Region)[levels(y$Region)=="MIC"] <- "Middle income"
  levels(y$Region)[levels(y$Region)=="NAC"] <- "North America"
  levels(y$Region)[levels(y$Region)=="INX"] <- "Not classified"
  levels(y$Region)[levels(y$Region)=="OED"] <- "OECD members"
  levels(y$Region)[levels(y$Region)=="OSS"] <- "Other small states"
  levels(y$Region)[levels(y$Region)=="PSS"] <- "Pacific island small states"
  levels(y$Region)[levels(y$Region)=="PST"] <- "Post-demographic dividend"
  levels(y$Region)[levels(y$Region)=="PRE"] <- "Pre-demographic dividend"
  levels(y$Region)[levels(y$Region)=="SST"] <- "Small states"
  levels(y$Region)[levels(y$Region)=="SAS"] <- "South Asia"
  levels(y$Region)[levels(y$Region)=="TSA"] <- "South Asia (IDA & IBRD)"
  levels(y$Region)[levels(y$Region)=="SSF"] <- "Sub-Saharan Africa"
  levels(y$Region)[levels(y$Region)=="SSA"] <- "Sub-Saharan Africa (excluding high income)"
  levels(y$Region)[levels(y$Region)=="TSS"] <- "Sub-Saharan Africa (IDA & IBRD countries)"
  levels(y$Region)[levels(y$Region)=="UMC"] <- "Upper middle income"
  levels(y$Region)[levels(y$Region)=="WLD"] <- "World"
  levels(y$Region)[levels(y$Region)==""] <- ""
  #	levels(y$Region)[levels(y$Region)==""] <- "Belgium-Luxembourg"
  #	levels(y$Region)[levels(y$Region)==""] <- "Czechoslovakia"
  #	levels(y$Region)[levels(y$Region)==""] <- "Ethiopia PDR"
  levels(y$Region)[levels(y$Region)=="VAT"] <- "Holy See"
  levels(y$Region)[levels(y$Region)=="MTQ"] <- "Martinique"
  levels(y$Region)[levels(y$Region)=="MSR"] <- "Montserrat"
  levels(y$Region)[levels(y$Region)=="ANT"] <- "Netherlands Antilles (former)"
  levels(y$Region)[levels(y$Region)=="NIU"] <- "Niue"
  levels(y$Region)[levels(y$Region)=="NFK"] <- "Norfolk Island"
  #	levels(y$Region)[levels(y$Region)==""] <- "Pacific Islands Trust Territory"
  levels(y$Region)[levels(y$Region)=="PCN"] <- "Pitcairn"
  levels(y$Region)[levels(y$Region)=="REU"] <- "Reunion"
  levels(y$Region)[levels(y$Region)=="SCG"] <- "Serbia and Montenegro"
  levels(y$Region)[levels(y$Region)=="SHN"] <- "Saint Helena, Ascension and Tristan da Cunha"
  levels(y$Region)[levels(y$Region)=="SPM"] <- "Saint Pierre and Miquelon"
  #	levels(y$Region)[levels(y$Region)=="ESH"] <- "Western Sahara"
  #	levels(y$Region)[levels(y$Region)==""] <- "Sudan (former)"
  levels(y$Region)[levels(y$Region)=="TWN"] <- "China, Taiwan Province of"
  levels(y$Region)[levels(y$Region)=="TKL"] <- "Tokelau"
  #	levels(y$Region)[levels(y$Region)==""] <- "USSR"
  levels(y$Region)[levels(y$Region)=="WLF"] <- "Wallis and Futuna Islands"
  #	levels(y$Region)[levels(y$Region)==""] <- "Yugoslav SFR"
  levels(y$Region)[levels(y$Region)=="AIA"] <- "Anguilla"
  #	levels(y$Region)[levels(y$Region)=="CHA"] <- "Channel Islands"
  #	levels(y$Region)[levels(y$Region)=="SJM"] <- "Svalbard and Jan Mayen Islands"
  #	levels(y$Region)[levels(y$Region)=="MYT"] <- "Mayotte"
  levels(y$Region)[levels(y$Region)=="CPR"] <- "China"
  
  z <- y
  return(z)
}

fscename1 <- function(y){
  
  levels(y$Scenario)[levels(y$Scenario)=="SSP2_500C_CACN_DAC_NoCC"] <- "BaU"
  levels(y$Scenario)[levels(y$Scenario)=="SSP2_500C_CACN_DAC_fdm_diet"] <- "1.5C"
  levels(y$Scenario)[levels(y$Scenario)=="SSP2_1100C_CACN_DAC_fdm_diet"] <- "2.0C"
  
z <- y
return(z)
}
fscename2 <- function(y){
  levels(y$Scenario)[levels(y$Scenario)=="SSP2_500C_CACN_DAC_NoCC"] <- "This study"
  levels(y$Scenario)[levels(y$Scenario)=="FAO-FAOTier1"] <- "FAOSTAT"
  levels(y$Scenario)[levels(y$Scenario)=="FAO-UNFCCC"] <- "NGHGI"
  levels(y$Scenario)[levels(y$Scenario)=="EDGAR"] <- "EDGAR"
  
  z <- y
  return(z)
}

fscename3 <- function(y){
  levels(y$Scenario)[levels(y$Scenario)=="SSP2-Baseline"] <- "Baseline"
  levels(y$Scenario)[levels(y$Scenario)=="SSP2-26"] <- "2.0C"
  levels(y$Scenario)[levels(y$Scenario)=="SSP2-19"] <- "1.5C"
  z <- y
  return(z)
}

fscename4 <- function(y){
  levels(y$Scenario)[levels(y$Scenario)=="SSP2_500C_CACN_DAC_NoCC"] <- "T"
  levels(y$Scenario)[levels(y$Scenario)=="FAO-FAOTier1"] <- "F"
  levels(y$Scenario)[levels(y$Scenario)=="FAO-UNFCCC"] <- "N"
  levels(y$Scenario)[levels(y$Scenario)=="EDGAR"] <- "E"
  
  z <- y
  return(z)
}

fscename5 <- function(y){
  levels(y$Scenario)[levels(y$Scenario)=="This study"] <- "This_study"
  z <- y
  return(z)
}

fmodname1 <- function(y){
  levels(y$Model)[levels(y$Model)=="AIMAFOLU"] <- "This study"
  levels(y$Model)[levels(y$Model)=="AIM/CGE 2.0"] <- "AIM-Hub standalone"
  levels(y$Model)[levels(y$Model)=="GCAM 4.2"] <- "GCAM4"
  levels(y$Model)[levels(y$Model)=="IMAGE 3.0.1"] <- "IMAGE"
  levels(y$Model)[levels(y$Model)=="MESSAGE-GLOBIOM 1.0"] <- "MESSAGE-GLOBIOM"
  levels(y$Model)[levels(y$Model)=="REMIND-MAgPIE 1.5"] <- "REMIND-MAgPIE"
  z <- y
  return(z)
}


#-----Data preparation

#system(paste("gams AFOLUVisualization.gms",sep=" "))

linepalette <- c("#4DAF4A","#377EB8","#E41A1C","#FF7F00","#984EA3","#8DD3C7","#F781BF","burlywood3","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","black","orangered3","orange","mediumblue","darkgreen","purple","grey","brown","#FFFF33")
linepalette2 <- c("#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","black","orangered3","orange","mediumblue","darkgreen","purple","grey","brown","#FFFF33","#4DAF4A","#377EB8","#E41A1C","#FF7F00","#984EA3","#8DD3C7","#F781BF","#A65628")
linepalette3 <- c("#F781BF","#8DD3C7","burlywood3","#4DAF4A","#377EB8","#E41A1C","#FF7F00","#984EA3","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","black","orangered3","orange","mediumblue","darkgreen","purple","grey","brown","#FFFF33")


AFOLUlist <- read.table("../individual/AFOLUCountry/variable_all.txt", header=F, sep=",") 
names(AFOLUlist) <- c("Variable2")
variablelist_emis <- read.table("../individual/AFOLUCountry/variable_emis.txt", header=T, sep=",")
variablelist_emis_nocc <- read.table("../individual/AFOLUCountry/variable_emis_nocc.txt", header=T, sep=",")
variablelist_emis_mit <- read.table("../individual/AFOLUCountry/variable_emis_mit.txt", header=T, sep=",")
variablelist_red <- read.table("../individual/AFOLUCountry/variable_red.txt", header=T, sep=",")
variablelist_emispn <- read.table("../individual/AFOLUCountry/variable_emispn.txt", header=T, sep=",")
variablelist_redpn <- read.table("../individual/AFOLUCountry/variable_redpn.txt", header=T, sep=",")

scenariolist <- read.table("../individual/AFOLUCountry/scenario.txt", header=T, sep=",")
scenariolist_luc_country <- read.table("../individual/AFOLUCountry/scenario_luc_country.txt", header=T, sep=",")

scenariolist_nocc <- read.table("../individual/AFOLUCountry/scenario_nocc.txt", header=T, sep=",")
scenariolist_mit <- read.table("../individual/AFOLUCountry/scenario_mit.txt", header=T, sep=",")
scenariolist_mit15C <- read.table("../individual/AFOLUCountry/scenario_mit15C.txt", header=T, sep=",")
scenariolist_mit2C <- read.table("../individual/AFOLUCountry/scenario_mit2C.txt", header=T, sep=",")

R5list <- read.table("../individual/AFOLUCountry/Region5.txt", header=T, sep=",")
R5pluswld <- read.table("../individual/AFOLUCountry/Region5pluswld.txt", header=T, sep=",")
R5pluslist <- read.table("../individual/AFOLUCountry/Region5plus.txt", header=T, sep=",")
R17list <- read.table("../individual/AFOLUCountry/Region17.txt", header=T, sep=",")
RISOlist <- read.table("../individual/AFOLUCountry/RISO.txt", header=T, sep=",")

#--- data load
#AIM/AFOLU
colnames <- c("Scenario","Region","Variable2","Year","Value")
data <- rgdx.param('../../output/gdx/IAMC_templatePlus_open.gdx','IAMC_templatePlus')
names(data) <- colnames

RISOlist <- read.table("../individual/AFOLUCountry/RISO.txt", header=T, sep=",")
worlddata_AFOLU <- filter(data,Region %in% c("World"))%>% inner_join(AFOLUlist,by=c("Variable2"))
R5data_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(R5list,by=c("Region"))
R17data_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(R17list,by=c("Region"))
RISOdata_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(RISOlist,by=c("Region"))


#-----Emissions - world - line

worlddata_Emis0.1 <- filter(worlddata_AFOLU) %>% inner_join(variablelist_emis,by=c("Variable2")) %>%
  inner_join(scenariolist_nocc,by=c("Scenario"))
worlddata_Emis0.1$Variable2 <- factor(worlddata_Emis0.1$Variable2, levels = c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Man_Soi","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_AFO_Lan_For_Bur","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir","Emi_CO2_Lan_Use_Flo_RemForest"))
worlddata_Emis0.1[1] <- lapply(worlddata_Emis0.1[1],as.factor)
worlddata_Emis0.1[3] <- lapply(worlddata_Emis0.1[3],as.factor)
worlddata_Emis0.1.1 <- fvarname1(worlddata_Emis0.1)
worlddata_Emis0.1.1 <- fscename1(worlddata_Emis0.1.1)

worlddata_Emis0.2 <- filter(worlddata_AFOLU) %>% inner_join(variablelist_emis_mit,by=c("Variable2")) %>% 
  inner_join(scenariolist_mit,by=c("Scenario"))
worlddata_Emis0.3 <- filter(worlddata_AFOLU) %>% inner_join(variablelist_emis_nocc,by=c("Variable2")) %>% 
  inner_join(scenariolist_nocc,by=c("Scenario"))
worlddata_Emis0.4 <- rbind(worlddata_Emis0.2,worlddata_Emis0.3)

worlddata_Emis0.4[1] <- lapply(worlddata_Emis0.4[1],as.factor)
worlddata_Emis0.4[3] <- lapply(worlddata_Emis0.4[3],as.factor)
worlddata_Emis0.4.1 <- fvarname2(worlddata_Emis0.4)
worlddata_Emis0.4.1 <- fscename1(worlddata_Emis0.4.1)

worlddata_Emis0.4.1$Scenario <- factor(worlddata_Emis0.4.1$Scenario, levels = c("BaU","2.0C","1.5C"))

linetype_pal1 <- c("Baseline"="solid","Baseline after translation"="dashed","Climate policy"="solid","Climate policy after translation"="dashed")
linecol_pal2 <- c("Baseline"="blue","1.5C"="orange","2.0C"="red","BaU"="black")

g1 <- ggplot() +
  geom_bar(data=subset(worlddata_Emis0.1.1), aes(x=Year,y=Value/1000,fill=Variable2), position="stack", stat="identity") +
  geom_line(data=subset(worlddata_Emis0.4.1), aes(x=Year,y=Value/1000,group=interaction(Scenario,Variable2), colour=Scenario, linetype = Variable2),size=0.8) +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Emissions (positive) or\nsequestration (negative) [Gt CO2eq/yr]") + xlab("Year") + MyThemeLine_grid3 +
  scale_size_manual(values=c(2, 2)) +
  theme(legend.position="left", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette) +
  scale_linetype_manual(values=linetype_pal1) +
  scale_colour_manual(values=linecol_pal2) +
  #  scale_fill_manual(values=linecol_pal1) +
  scale_x_discrete(breaks=c("2005","2050","2100"))

plot(g1)

outname <- paste0("../../output/fig/Fig6a_emis_world_line.png")
ggsave(g1, file=outname, dpi = 600, width=7, height=4,limitsize=FALSE)

#-----Emissions - global R5 breakdown - line
#worlddata_Emis0.2 <- filter(worlddata_AFOLU, Variable2 %in% c("Emi_equ_GHG_AFO"))


R5data_Emis0.1 <- filter(R5data_AFOLU) %>% inner_join(variablelist_emispn,by=c("Variable2")) %>% 
  inner_join(scenariolist_nocc,by=c("Scenario"))
R5data_Emis0.1[1] <- lapply(R5data_Emis0.1[1],as.factor)
R5data_Emis0.1[2] <- lapply(R5data_Emis0.1[2],as.factor)
R5data_Emis0.1[3] <- lapply(R5data_Emis0.1[3],as.factor)
R5data_Emis0.1.1 <- fvarname1(R5data_Emis0.1)
R5data_Emis0.1.1 <- fregname1(R5data_Emis0.1)
R5data_Emis0.1.1 <- fscename1(R5data_Emis0.1.1)

g1 <- ggplot() +
  geom_bar(data=subset(R5data_Emis0.1.1), aes(x=Year,y=Value/1000,fill=Region), position="stack", stat="identity") +
  #  geom_line(data=subset(worlddata_Emis0.2.1), aes(x=Year,y=Value/1000,group=Scenario), colour=NA, linetype="solid",size=0.8) +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Gt CO2eq/yr") + xlab("Year") + MyThemeLine_grid3 +
  scale_size_manual(values=c(2, 2))+
  theme(legend.position="right", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette2) +
  scale_x_discrete(breaks=c("2005","2050","2100"))

plot(g1)
outname <- paste0("../../output/fig/Fig6b_emis_r5_tot.png")
ggsave(g1, file=outname, dpi = 600, width=6, height=4,limitsize=FALSE)

#-----Emissions reduction time series - world

worlddata_Emis0.2 <- filter(worlddata_AFOLU) %>% inner_join(variablelist_red,by=c("Variable2")) %>% inner_join(scenariolist_mit,by=c("Scenario"))
worlddata_Emis0.2$Variable2 <- factor(worlddata_Emis0.2$Variable2, levels = c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_Red_equ_CH4_AFO_Agr_Ric","Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_Red_equ_N2O_AFO_Agr_Man_Soi","Emi_Red_CO2_AFO_Lan_Pro","Emi_Red_CO2_AFO_Lan_Res","Emi_Red_CO2_AFO_Lan_Man","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch",
                                                                              "Car_Seq_Lan_Use_Soi_Car_Man","Car_Seq_Lan_Use_Soi_Car_Man_Cro","Car_Seq_Lan_Use_Soi_Car_Man_Gra","Emi_CO2_AFO_Aff","Emi_Red_GHG_AFO_Die_Cha"))
worlddata_Emis0.2[1] <- lapply(worlddata_Emis0.2[1],as.factor)
worlddata_Emis0.2[3] <- lapply(worlddata_Emis0.2[3],as.factor)
worlddata_Emis0.2.1 <- fvarname1(worlddata_Emis0.2)
worlddata_Emis0.2.1 <- fscename1(worlddata_Emis0.2.1)

g2 <- ggplot() +
  geom_bar(data=subset(worlddata_Emis0.2.1), aes(x=Year,y=Value/1000,fill=Variable2), position="stack", stat="identity") +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Emission reduction (positive values) or\n carbon sequestation (negative values)[Gt CO2eq/yr]") + xlab("Year") + MyThemeLine_grid3 +
  theme(legend.position="right", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette) +
  scale_x_discrete(breaks=c("2030","2050","2100"))

g2.1 <- g2+facet_wrap(Scenario~.)
plot(g2.1)

outname <- paste0("../../output/fig/Fig7a_red_world.png")
ggsave(g2.1, file=outname, dpi = 600, width=8, height=4,limitsize=FALSE)


#-----Emissions reduction time series - global with R5 breakdown


R5data_Emis0.2 <- filter(R5data_AFOLU) %>% inner_join(scenariolist_mit,by=c("Scenario")) %>% inner_join(variablelist_redpn,by=c("Variable2"))
R5data_Emis0.2$Variable2 <- factor(R5data_Emis0.2$Variable2, levels = c("Emi_GHG_AFO_Red","Emi_GHG_AFO_Seq"))

R5data_Emis0.2[1] <- lapply(R5data_Emis0.2[1],as.factor)
R5data_Emis0.2[2] <- lapply(R5data_Emis0.2[2],as.factor)
R5data_Emis0.2[3] <- lapply(R5data_Emis0.2[3],as.factor)
R5data_Emis0.2.1 <- fvarname1(R5data_Emis0.2)
R5data_Emis0.2.1 <- fregname1(R5data_Emis0.2.1)
R5data_Emis0.2.1 <- fscename1(R5data_Emis0.2.1)

g4 <- ggplot() +
  geom_bar(data=subset(R5data_Emis0.2.1), aes(x=Year,y=Value/1000,fill=Region), position="stack", stat="identity") +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Emission reduction (positive values) or\n carbon sequestation (negative values) [Gt CO2eq/yr]") + xlab("Year") + MyThemeLine_grid3 +
  theme(legend.position="right", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette2) +
  scale_x_discrete(breaks=c("2030","2050","2100"))

g4.1 <- g4+facet_wrap(Scenario~.)
plot(g4.1)

outname <- paste0("../../output/fig/Fig7b_red_r5_tot.png")
ggsave(g4.1, file=outname, dpi = 600, width=8, height=4,limitsize=FALSE)

#-----Emissions reduction time series - R5
R5data_Emis0.2 <- filter(R5data_AFOLU, Year %in% c("2050","2100")) %>% inner_join(scenariolist_mit,by=c("Scenario")) %>% inner_join(variablelist_red,by=c("Variable2"))
R5data_Emis0.2$Variable2 <- factor(R5data_Emis0.2$Variable2, levels = c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_Red_equ_CH4_AFO_Agr_Ric","Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_Red_equ_N2O_AFO_Agr_Man_Soi","Emi_Red_CO2_AFO_Lan_Pro","Emi_Red_CO2_AFO_Lan_Res","Emi_Red_CO2_AFO_Lan_Man","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch",
                                                                        "Car_Seq_Lan_Use_Soi_Car_Man","Car_Seq_Lan_Use_Soi_Car_Man_Cro","Car_Seq_Lan_Use_Soi_Car_Man_Gra","Emi_CO2_AFO_Aff","Emi_Red_GHG_AFO_Die_Cha"))
R5data_Emis0.2[1] <- lapply(R5data_Emis0.2[1],as.factor)
R5data_Emis0.2[2] <- lapply(R5data_Emis0.2[2],as.factor)
R5data_Emis0.2[3] <- lapply(R5data_Emis0.2[3],as.factor)
R5data_Emis0.2.1 <- fvarname1(R5data_Emis0.2)
R5data_Emis0.2.1 <- fregname1(R5data_Emis0.2.1)
R5data_Emis0.2.1 <- fscename1(R5data_Emis0.2.1)

g4 <- ggplot() +
  geom_bar(data=subset(R5data_Emis0.2.1), aes(x=Region,y=Value/1000,fill=Variable2), position="stack", stat="identity") +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Gt CO2eq/yr") + xlab("") + MyThemeLine_grid2 +
  theme(legend.position="right", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette)
g4.1 <- g4+facet_grid(Scenario~Year)
plot(g4.1)

outname <- paste0("../../output/fig/Fig7c_red_r5.png")
ggsave(g4.1, file=outname, dpi = 600, width=6, height=7,limitsize=FALSE)




#-----Emissions - RISO - line
RISOlist <- read.table("../individual/AFOLUCountry/RISO_top100fao.txt", header=T, sep=",")
RISOdata_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(RISOlist,by=c("Region"))

RISOdata_Emis0.1 <- filter(RISOdata_AFOLU) %>% inner_join(variablelist_emis,by=c("Variable2")) %>% inner_join(scenariolist_nocc,by=c("Scenario"))
RISOdata_Emis0.1$Variable2 <- factor(RISOdata_Emis0.1$Variable2, levels = c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Man_Soi","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_AFO_Lan_For_Bur","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir","Emi_CO2_Lan_Use_Flo_RemForest"))
RISOdata_Emis0.1[1] <- lapply(RISOdata_Emis0.1[1],as.factor)
RISOdata_Emis0.1[2] <- lapply(RISOdata_Emis0.1[2],as.factor)
RISOdata_Emis0.1[3] <- lapply(RISOdata_Emis0.1[3],as.factor)
RISOdata_Emis0.1.1 <- fvarname1(RISOdata_Emis0.1)
RISOdata_Emis0.1.1 <- fscename1(RISOdata_Emis0.1.1)
RISOdata_Emis0.1.1 <- fregname2(RISOdata_Emis0.1.1)

RISOdata_Emis0.2 <- filter(RISOdata_AFOLU) %>% inner_join(variablelist_emis_mit,by=c("Variable2")) %>%
  inner_join(scenariolist_mit,by=c("Scenario"))
RISOdata_Emis0.3 <- filter(RISOdata_AFOLU) %>% inner_join(variablelist_emis_nocc,by=c("Variable2")) %>% 
  inner_join(scenariolist_nocc,by=c("Scenario"))
RISOdata_Emis0.4 <- rbind(RISOdata_Emis0.2,RISOdata_Emis0.3)

RISOdata_Emis0.4[1] <- lapply(RISOdata_Emis0.4[1],as.factor)
RISOdata_Emis0.4[2] <- lapply(RISOdata_Emis0.4[2],as.factor)
RISOdata_Emis0.4[3] <- lapply(RISOdata_Emis0.4[3],as.factor)
RISOdata_Emis0.4.1 <- fvarname2(RISOdata_Emis0.4)
RISOdata_Emis0.4.1 <- fscename1(RISOdata_Emis0.4.1)
RISOdata_Emis0.4.1 <- fregname2(RISOdata_Emis0.4.1)

RISOdata_Emis0.1.1$Region <- factor(RISOdata_Emis0.1.1$Region, levels = c("Afghanistan","Algeria","Angola","Argentina","Australia","Austria","Bangladesh","Belarus","Belgium","Benin","Bolivia","Botswana","Brazil","Bulgaria","Burkina\nFaso","Cambodia","Cameroon","Canada","Central\nAfrican Rep.","Chad","Chile","China","Colombia","Congo D.R.","Cote\nd'Ivoire","Cuba","Denmark","Dominican\nRep.","Ecuador","Egypt","Ethiopia","Finland","France","Germany","Ghana","Greece","Guatemala","Guinea","Honduras","India","Indonesia","Iran","Ireland","Italy","Japan","Kazakhstan","Kenya","Korea, DPR.","Lao PDR","Liberia","Madagascar","Malawi","Malaysia","Mali","Mauritania","Mexico","Mongolia","Morocco","Mozambique","Myanmar","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Pakistan","Panama","Paraguay","Peru","Philippines","Poland","Portugal","Romania","Russia","Senegal","Somalia","South\nAfrica","South\nKorea","Spain","Sri Lanka","Sweden","Switzerland","Syria","Tanzania","Thailand","Turkey","Turkmenistan","Uganda","Ukraine","United\nKingdom","United\nStates","Uruguay","Uzbekistan","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe"))  
RISOdata_Emis0.4.1$Region <- factor(RISOdata_Emis0.4.1$Region, levels = c("Afghanistan","Algeria","Angola","Argentina","Australia","Austria","Bangladesh","Belarus","Belgium","Benin","Bolivia","Botswana","Brazil","Bulgaria","Burkina\nFaso","Cambodia","Cameroon","Canada","Central\nAfrican Rep.","Chad","Chile","China","Colombia","Congo D.R.","Cote\nd'Ivoire","Cuba","Denmark","Dominican\nRep.","Ecuador","Egypt","Ethiopia","Finland","France","Germany","Ghana","Greece","Guatemala","Guinea","Honduras","India","Indonesia","Iran","Ireland","Italy","Japan","Kazakhstan","Kenya","Korea, DPR.","Lao PDR","Liberia","Madagascar","Malawi","Malaysia","Mali","Mauritania","Mexico","Mongolia","Morocco","Mozambique","Myanmar","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Pakistan","Panama","Paraguay","Peru","Philippines","Poland","Portugal","Romania","Russia","Senegal","Somalia","South\nAfrica","South\nKorea","Spain","Sri Lanka","Sweden","Switzerland","Syria","Tanzania","Thailand","Turkey","Turkmenistan","Uganda","Ukraine","United\nKingdom","United\nStates","Uruguay","Uzbekistan","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe"))  


g1 <- ggplot() +
  geom_bar(data=subset(RISOdata_Emis0.1.1), aes(x=Year,y=Value/1000,fill=Variable2), position="stack", stat="identity") +
  geom_line(data=subset(RISOdata_Emis0.4.1), aes(x=Year,y=Value/1000,group=interaction(Scenario,Variable2), colour=Scenario, linetype = Variable2)) +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Land-based emissions [Gt CO2eq/yr]") + xlab("Year") + MyThemeLine_grid3 +
  scale_size_manual(values=c(2, 2))+
  guides(fill=guide_legend(ncol =2)) +
  guides(color=guide_legend(ncol =1)) +
  scale_fill_manual(values=linepalette) +
  scale_colour_manual(values=linecol_pal2) +
  scale_linetype_manual(values=linetype_pal1) +
  scale_x_discrete(breaks=c("2020","2100")) +
  theme(legend.position="bottom",
        text=element_text(size=12),
        legend.title=element_blank(),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(0, 0, 0, 0),"cm"),
        strip.text.x = element_text(margin = margin(0, 0, 0, 0))
  )
  
g1.1 <- g1+facet_wrap(Region~., scales = "free_y",ncol = 6)
#plot(g1.1)

outname <- paste0("../../output/fig/Fig8_emis_riso_line.png")
ggsave(g1.1, file=outname, dpi = 600, width=10, height=17,limitsize=FALSE)

#-----Emissions reduction time series - RISO

RISOlist <- read.table("../individual/AFOLUCountry/RISO_top100fao.txt", header=T, sep=",")
RISOdata_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(RISOlist,by=c("Region"))

RISOdata_Emis0.2 <- filter(RISOdata_AFOLU) %>% inner_join(variablelist_red,by=c("Variable2"))%>% inner_join(RISOlist,by=c("Region")) %>%
  inner_join(scenariolist_mit15C,by=c("Scenario"))
RISOdata_Emis0.2$Variable2 <- factor(RISOdata_Emis0.2$Variable2, levels = c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_Red_equ_CH4_AFO_Agr_Ric","Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_Red_equ_N2O_AFO_Agr_Man_Soi","Emi_Red_CO2_AFO_Lan_Pro","Emi_Red_CO2_AFO_Lan_Res","Emi_Red_CO2_AFO_Lan_Man","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Enr",
                                                                            "Car_Seq_Lan_Use_Soi_Car_Man","Car_Seq_Lan_Use_Soi_Car_Man_Cro","Car_Seq_Lan_Use_Soi_Car_Man_Gra","Emi_CO2_AFO_Aff","Emi_Red_GHG_AFO_Die_Cha","Emi_Red_CO2_AFO_Agr_Seq"))

RISOdata_Emis0.2[2] <- lapply(RISOdata_Emis0.2[2],as.factor)
RISOdata_Emis0.2[3] <- lapply(RISOdata_Emis0.2[3],as.factor)
RISOdata_Emis0.2.1 <- fvarname1(RISOdata_Emis0.2)
RISOdata_Emis0.2.2 <- fregname1(RISOdata_Emis0.2.1)
RISOdata_Emis0.2.2 <- fregname2(RISOdata_Emis0.2.2)
#RISOdata_Emis0.2.2 <- fscename2(RISOdata_Emis0.2.2)

RISOdata_Emis0.2.2$Region <- factor(RISOdata_Emis0.2.2$Region, levels = c("Afghanistan","Algeria","Angola","Argentina","Australia","Austria","Bangladesh","Belarus","Belgium","Benin","Bolivia","Botswana","Brazil","Bulgaria","Burkina\nFaso","Cambodia","Cameroon","Canada","Central\nAfrican Rep.","Chad","Chile","China","Colombia","Congo D.R.","Cote\nd'Ivoire","Cuba","Denmark","Dominican\nRep.","Ecuador","Egypt","Ethiopia","Finland","France","Germany","Ghana","Greece","Guatemala","Guinea","Honduras","India","Indonesia","Iran","Ireland","Italy","Japan","Kazakhstan","Kenya","Korea, DPR.","Lao PDR","Liberia","Madagascar","Malawi","Malaysia","Mali","Mauritania","Mexico","Mongolia","Morocco","Mozambique","Myanmar","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Pakistan","Panama","Paraguay","Peru","Philippines","Poland","Portugal","Romania","Russia","Senegal","Somalia","South\nAfrica","South\nKorea","Spain","Sri Lanka","Sweden","Switzerland","Syria","Tanzania","Thailand","Turkey","Turkmenistan","Uganda","Ukraine","United\nKingdom","United\nStates","Uruguay","Uzbekistan","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe"))  


g8 <- ggplot() +
  geom_bar(data=subset(RISOdata_Emis0.2.2), aes(x=Year,y=Value/1000,fill=Variable2), position="stack", stat="identity") +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Emission reduction (positive values) and carbon sequestration (negative values) [Gt CO2eq/yr]") + xlab("Year") + MyThemeLine_grid3 +
  theme(legend.position="bottom", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10),
        plot.margin = unit(c(0, 0, 0, 0),"cm"),
        strip.text.x = element_text(margin = margin(0, 0, 0, 0))
  ) +
  scale_fill_manual(values=linepalette) +
  scale_x_discrete(breaks=c("2020","2100")) +
  guides(fill=guide_legend(ncol =3)) +
  guides(color=guide_legend(ncol =1))
g8.1 <- g8+facet_wrap(Region~., scales = "free_y",ncol = 7)
#plot(g8.1)

outname <- paste0("../../output/fig/Fig9_red_riso_15C.png")
ggsave(g8.1, file=outname, dpi = 600, width=10, height=16,limitsize=FALSE)

#-----Emissions comparisons - R5
R5data_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(R5pluswld,by=c("Region"))
R5data_Emis0.4 <- filter(R5data_AFOLU,Year %in% c("2005")) %>% inner_join(variablelist_emis,by=c("Variable2")) %>% inner_join(scenariolist_luc_country,by=c("Scenario"))
R5data_Emis0.4 <- R5data_Emis0.4[!(R5data_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
R5data_Emis0.4[2] <- lapply(R5data_Emis0.4[2],as.factor)
R5data_Emis0.4[1] <- lapply(R5data_Emis0.4[1],as.factor)
R5data_Emis0.4[3] <- lapply(R5data_Emis0.4[3],as.factor)
R5data_Emis0.4$Region <- factor(R5data_Emis0.4$Region, levels = c("World","R5OECD90+EU","R5REF","R5ASIA","R5MAF","R5LAM"))
R5data_Emis0.4$Variable2 <- factor(R5data_Emis0.4$Variable2, levels = c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir","Emi_CO2_AFO_Lan_For_Bur","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Man_Soi"))
R5data_Emis0.4.1 <- fvarname1(R5data_Emis0.4)
R5data_Emis0.4.2 <- fregname2(R5data_Emis0.4.1)
R5data_Emis0.4.2 <- fscename4(R5data_Emis0.4.2)
#  R5data_Emis0.4.2$Scenario <- factor(R5data_Emis0.4.2$Scenario, levels = c("This study","FAOSTAT","NGHGI","EDGAR","GLOBIOM","IMAGE","MAgPIE"))
R5data_Emis0.4.2$Scenario <- factor(R5data_Emis0.4.2$Scenario, levels = c("T","F","E","N"))

linepalette3 <- c("#F781BF","#8DD3C7","burlywood3","#4DAF4A","#377EB8","#E41A1C","#FF7F00","#984EA3","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","black","orangered3","orange","mediumblue","darkgreen","purple","grey","brown","#FFFF33")
# original fire and forest burning colour: "#A65628" and #F781BF
g12 <- ggplot() +
  geom_bar(data=subset(R5data_Emis0.4.2), aes(y=Value/1000, x=Scenario ,fill=Variable2), position="stack", stat="identity") +
  geom_hline(yintercept=0,linetype="solid") +
  geom_vline(xintercept=0,linetype="dashed") +
  xlab("") + ylab("Emissions and carbon \nsequestration [Gt CO2eq/year]") + MyThemeLine_grid2h +
  theme(legend.position="right", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10),
        plot.margin = unit(c(0, 0, 0, 0),"cm"),
        strip.text.x = element_text(margin = margin(0, 0, 0, 0))
        ) +
  scale_fill_manual(values=linepalette3)
g12.1 <- g12+facet_wrap(.~Region, scales = "free_y",nrow=1)
plot(g12.1)

outname <- paste0("../../output/fig/Fig2_emiscomparison_R5v2.png")
ggsave(g12.1, file=outname, dpi = 600, width=12, height=2.6,limitsize=FALSE)

#-----Emissions comparisons - RISO
RISOlist <- read.table("../individual/AFOLUCountry/RISO_top100fao.txt", header=T, sep=",")
RISOdata_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(RISOlist,by=c("Region"))
RISOdata_Emis0.4 <- filter(RISOdata_AFOLU,Year %in% c("2005")) %>% inner_join(variablelist_emis,by=c("Variable2")) %>% inner_join(RISOlist,by=c("Region")) %>% inner_join(scenariolist_luc_country,by=c("Scenario"))
RISOdata_Emis0.4 <- RISOdata_Emis0.4[!(RISOdata_Emis0.4$Region=="World" & RISOdata_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
RISOdata_Emis0.4 <- RISOdata_Emis0.4[!(RISOdata_Emis0.4$Region=="R5OECD90+EU" & RISOdata_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
RISOdata_Emis0.4 <- RISOdata_Emis0.4[!(RISOdata_Emis0.4$Region=="R5REF" & RISOdata_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
RISOdata_Emis0.4 <- RISOdata_Emis0.4[!(RISOdata_Emis0.4$Region=="R5ASIA" & RISOdata_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
RISOdata_Emis0.4 <- RISOdata_Emis0.4[!(RISOdata_Emis0.4$Region=="R5MAF" & RISOdata_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
RISOdata_Emis0.4 <- RISOdata_Emis0.4[!(RISOdata_Emis0.4$Region=="R5LAM" & RISOdata_Emis0.4$Scenario=="FAO-UNFCCC") ,c(1,2,3,4,5)]
RISOdata_Emis0.4[2] <- lapply(RISOdata_Emis0.4[2],as.factor)
RISOdata_Emis0.4[1] <- lapply(RISOdata_Emis0.4[1],as.factor)
RISOdata_Emis0.4[3] <- lapply(RISOdata_Emis0.4[3],as.factor)
RISOdata_Emis0.4$Variable2 <- factor(RISOdata_Emis0.4$Variable2, levels = c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir","Emi_CO2_AFO_Lan_For_Bur","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Man_Soi"))
RISOdata_Emis0.4.1 <- fvarname1(RISOdata_Emis0.4)
RISOdata_Emis0.4.2 <- fregname2(RISOdata_Emis0.4.1)
RISOdata_Emis0.4.2 <- fscename4(RISOdata_Emis0.4.2)

RISOdata_Emis0.4.2$Scenario <- factor(RISOdata_Emis0.4.2$Scenario, levels = c("T","F","N","E"))

RISOdata_Emis0.4.2$Region <- factor(RISOdata_Emis0.4.2$Region, levels = c("Afghanistan","Algeria","Angola","Argentina","Australia","Austria","Bangladesh","Belarus","Belgium","Benin","Bolivia","Botswana","Brazil","Bulgaria","Burkina\nFaso","Cambodia","Cameroon","Canada","Central\nAfrican Rep.","Chad","Chile","China","Colombia","Congo D.R.","Cote\nd'Ivoire","Cuba","Denmark","Dominican\nRep.","Ecuador","Egypt","Ethiopia","Finland","France","Germany","Ghana","Greece","Guatemala","Guinea","Honduras","India","Indonesia","Iran","Ireland","Italy","Japan","Kazakhstan","Kenya","Korea, DPR.","Lao PDR","Liberia","Madagascar","Malawi","Malaysia","Mali","Mauritania","Mexico","Mongolia","Morocco","Mozambique","Myanmar","Namibia","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Pakistan","Panama","Paraguay","Peru","Philippines","Poland","Portugal","Romania","Russia","Senegal","Somalia","South\nAfrica","South\nKorea","Spain","Sri Lanka","Sweden","Switzerland","Syria","Tanzania","Thailand","Turkey","Turkmenistan","Uganda","Ukraine","United\nKingdom","United\nStates","Uruguay","Uzbekistan","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe"))  

linepalette3 <- c("#F781BF","#8DD3C7","burlywood3","#4DAF4A","#377EB8","#E41A1C","#FF7F00","#984EA3","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","black","orangered3","orange","mediumblue","darkgreen","purple","grey","brown","#FFFF33")

# original fire and forest burning colour: "#A65628" and #F781BF
g12 <- ggplot() +
  geom_bar(data=subset(RISOdata_Emis0.4.2), aes(y=Value/1000, x=Scenario ,fill=Variable2), position="stack", stat="identity") +
  geom_hline(yintercept=0,linetype="solid") +
  geom_vline(xintercept=0,linetype="dashed") +
  xlab("") + ylab("Emission reduction (positive values) and carbon sequestation (negative values) [Gt CO2eq/yr]") + MyThemeLine_grid2h +
  theme(legend.position="bottom", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10),
        plot.margin = unit(c(0, 0, 0, 0),"cm"),
        strip.text.x = element_text(margin = margin(0, 0, 0, 0))
        ) +
  scale_fill_manual(values=linepalette3) +
  guides(fill=guide_legend(ncol =2)) +
  guides(color=guide_legend(ncol =1))
g12.1 <- g12+facet_wrap(.~Region, scales = "free_y",ncol=8)
#plot(g12.1)

outname <- paste0("../../output/fig/Fig3_emiscomparison_riso.png")
ggsave(g12.1, file=outname, dpi = 600, width=11, height=15,limitsize=FALSE)


#-----Emissions comparisons - RISO scatter
#Yplot <- c("2005")
RISOlist <- read.table("../individual/AFOLUCountry/RISO.txt", header=T, sep=",")
RISOdata_AFOLU <- filter(data) %>% inner_join(AFOLUlist,by=c("Variable2"))%>% inner_join(RISOlist,by=c("Region"))
RISOdata_Emis0.4.0<- filter(RISOdata_AFOLU,Year %in% c("2005")) %>% inner_join(scenariolist_luc_country,by=c("Scenario"))

RISOdata_Emis0.4 <- filter(RISOdata_Emis0.4.0,Variable2 %in% c("Emi_equ_GHG_AFO_Agr","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For"))
outname <- paste0("../../output/fig/Fig4_plot_scatter_RISO.png")
heightvar <- 5
maxv <- 200
minv <- -100

RISOdata_Emis0.4[2] <- lapply(RISOdata_Emis0.4[2],as.factor)
RISOdata_Emis0.4[1] <- lapply(RISOdata_Emis0.4[1],as.factor)
RISOdata_Emis0.4[3] <- lapply(RISOdata_Emis0.4[3],as.factor)
RISOdata_Emis0.4$Variable2 <- factor(RISOdata_Emis0.4$Variable2, levels = c("Emi_equ_GHG_AFO_Agr","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For"))

RISOdata_Emis0.4.1 <- fvarname1(RISOdata_Emis0.4)
RISOdata_Emis0.4.2 <- fregname2(RISOdata_Emis0.4.1)
RISOdata_Emis0.4.2 <- fscename2(RISOdata_Emis0.4.2)
RISOdata_Emis0.4.2 <- fscename5(RISOdata_Emis0.4.2)

RISOdata_Emis0.4.3 <- RISOdata_Emis0.4.2 %>% spread(key=Scenario, value=Value, fill=NA)
#RISOdata_Emis0.4.3 <- na.omit(RISOdata_Emis0.4.3)
RISOdata_Emis0.4.3 <- RISOdata_Emis0.4.3 %>% gather(key=Scenario,value=Value,-Region,-Variable2,-Year,-"This_study")
RISOdata_Emis0.4.3$Value <- as.numeric(RISOdata_Emis0.4.3$Value)
RISOdata_Emis0.4.3$This_study <- as.numeric(RISOdata_Emis0.4.3$This_study)


data2 <- data.frame(x = minv:maxv)
data2$y <- data2$x

g12 <- ggplot() +
  geom_point(data=subset(RISOdata_Emis0.4.3), aes(x=This_study,y=Value), stat="identity",size=0.7) +
  geom_line(data=data2,aes(x=x,y=y)) +
  xlim(minv, maxv) +
  ylim(minv, maxv) +
  ylab("Other estimates [Mt CO2eq/yr]") + 
  xlab("This study's estimates [Mt CO2eq/yr]") + MyThemeLine_grid2h +
  theme(legend.position="right", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette3)
g12.1 <- g12+facet_grid(Variable2~Scenario, scales = "free")
plot(g12.1)

outname <- paste0("../../output/fig/Fig4_scatter.png")
ggsave(g12.1, file=outname, dpi = 600, width=5, height=heightvar,limitsize=FALSE)


#-----Emissions - world - line AR6

#AR6 data
colnames <- c("Model","Scenario","Region","Variable2","Year","Value")
DataAR6 <- rgdx.param('../../output/gdx/IAMC_templatePlus_open.gdx','Emi_equ_GHG_AFO_AR6')
names(DataAR6) <- colnames

DataAR6 <- filter(DataAR6,Variable2 %in% c("Emi_equ_GHG_AFO","Emi_GHG_AFO_Seq","Emi_CO2_AFO_Aff")) 
DataAR6[1] <- lapply(DataAR6[1],as.factor)
DataAR6[2] <- lapply(DataAR6[2],as.factor)
DataAR6[4] <- lapply(DataAR6[4],as.factor)
DataAR6 <- fmodname1(DataAR6)
DataAR6 <- fvarname1(DataAR6)
DataAR6 <- fscename3(DataAR6)
DataAR6$Model <- factor(DataAR6$Model, levels = c("This study","AIM-Hub standalone","GCAM4","IMAGE","MESSAGE-GLOBIOM","REMIND-MAgPIE"))

DataAR6$Variable2 <- factor(DataAR6$Variable2, levels = c("Emissions - Agr + LUC","Land - Sink","Afforestation"))

g1 <- ggplot() +
#  geom_line(data=subset(worlddata_Emis0.2.1), aes(x=Year,y=Value/1000,group=Scenario, colour=Scenario), linetype="solid",size=0.8) +
  geom_line(data=subset(DataAR6), aes(x=Year,y=Value/1000,group=interaction(Model,Scenario), colour=Scenario, linetype = Model),size=0.8) +
  geom_hline(yintercept=0,linetype="solid") +
  ylab("Gt CO2eq/yr") + xlab("Year") + MyThemeLine_grid3 +
  scale_size_manual(values=c(2, 2))+
  theme(legend.position="left", text=element_text(size=12),legend.title=element_blank(),legend.text = element_text(size = 10)) +
  scale_fill_manual(values=linepalette) +
  scale_colour_manual(values=linecol_pal2) +
  scale_x_discrete(breaks=c("2010","2025","2050","2075","2100"))
g1.1 <- g1+facet_wrap(Variable2~.)
plot(g1.1)

outname <- paste0("../../output/fig/Fig5_emis_world_line_AR6.png")
ggsave(g1.1, file=outname, dpi = 600, width=7.5, height=4,limitsize=FALSE)



