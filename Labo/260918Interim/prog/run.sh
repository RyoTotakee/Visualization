#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

Rscript prog/01_basin_yield_maps.R
Rscript prog/01b_basin_yield_by_commodity.R
Rscript prog/02_grid_land_use_maps.R
Rscript prog/03_country_indicators.R
Rscript prog/03b_country_by_commodity.R
Rscript prog/03c_region_world_indicators.R
Rscript prog/04_land_emissions.R
Rscript prog/05_biodiversity.R
Rscript prog/06a_protection_land_yield.R
Rscript prog/06b_protection_agriculture.R
Rscript prog/06c_protection_outcomes.R
Rscript prog/06d_protection_spatial_tradeoffs.R
Rscript prog/07a_conditional_protection_agriculture.R
Rscript prog/07b_conditional_protection_land_channels.R
Rscript prog/08_land_use_protection_overlap.R
Rscript prog/09_land_dynamics_story.R
Rscript prog/10_country_opportunity_profiles.R
Rscript prog/11_results_evidence_chain.R
Rscript prog/12_simple_overviews.R
Rscript prog/13_agmip_area_yield_decomposition.R
Rscript prog/14_luc_emissions_balance.R
Rscript prog/15_spatial_reallocation_bivariate.R
