# GEER2026 Working Notes

## Overview
- This folder is a paper-specific analysis workspace for GEER 2026.
- Main focus is integrated AFOLU analysis using AIM-ALPHA, AFOLUB, and optionally AIM-PLUM.
- Current exploratory scripts are `prog/1st_check.R` and `prog/2nd_check.R`.
- Main input snapshot currently used is `data/0311/`.

## Folder Structure
- `prog/`
  - R scripts for exploratory analysis and figure generation.
  - `1st_check.R` and `2nd_check.R` are the current working scripts.
- `data/`
  - Large local input datasets.
  - `0311/` contains the current working GDX files.
- `define/`
  - Variable and item definitions and scope-setting files.
  - `AGMIP.set` explains ALPHA-side AgMIP variable names, items, and units.
  - `IAMC.set` explains IAMC-style variable names used mainly by AFOLUB and PLUM outputs.
  - `country.set` is the country coverage list that should be treated as the default analysis universe.
- `doc/`
  - Research framing, outline, and study ideas in Markdown.
  - Use this first to understand intended paper story before adding figures.
- `output/`
  - Generated figures, summaries, and temporary inspection outputs.
  - `output/0311/` is the current destination for `1st_check.R` and `2nd_check.R`.

## Country Coverage Rule
- Default country coverage should come from `define/country.set`.
- Do not infer the target country list from the GDX contents alone.
- Reason:
  - different GDX files include different country sets
  - some files mix countries and aggregated regions
  - relying on raw GDX coverage can silently introduce regional aggregates into country plots
- Recommended practice:
  - read `define/country.set`
  - restrict analysis tables to those ISO3 codes
  - treat `World` separately only when needed for totals or shares
  - when making maps, join the map layer to `country.set`-filtered tables rather than the reverse

## Main Data Files
- `data/0311/AGMIP_all_scenario.gdx`
  - AIM-ALPHA output.
  - Very large, around 888 MB.
  - Useful for country-level agricultural variables, producer prices, emissions, calories, land use, production, and detailed AgMIP items.
- `data/0311/IAMC_templatePlus.gdx`
  - AFOLUB output.
  - Around 30 MB.
  - Useful for mitigation cost, AFOLU reduction, agricultural production and price indicators under mitigation cases.
- `data/0311/Mergedcomparison.gdx`
  - AIM-PLUM and IAMC-style merged results.
  - Around 202 MB.
  - Useful for baseline comparison, land cover, land-use emissions and sinks, land allocation, and some IAMC harmonized variables.

## Data Priority By Topic
- For ALPHA-side agricultural variables such as prices, production, calories, and some agricultural emissions:
  - first look in `AGMIP_all_scenario.gdx`
- For AFOLUB-side mitigation costs, CH4/N2O/CO2 reduction amounts, and residual agricultural emissions:
  - use `IAMC_templatePlus.gdx`
- For LULUCF-side land-use emissions, sinks, net emissions, land-cover change, and land allocation:
  - prefer `Mergedcomparison.gdx`
- In particular, for this project, the following should generally come from `Mergedcomparison.gdx` rather than reconstructed approximations:
  - land-use CO2 emissions
  - land-use CO2 sinks / sequestration
  - net LULUCF emissions
  - land-use / land-cover change

## Land Variable Handling Rule
- Land-side variables in `IAMC_templatePlus.gdx` are easy to mix up. Treat them in three distinct groups.
- Baseline land state:
  - `Lan_Cov_*`
  - these are land-cover stocks, not mitigation amounts
- Baseline land emissions / sinks / flows:
  - `Emi_CO2_AFO_Lan_*`
  - `Emi_equ_CH4_AFO_Lan_*`
  - `Emi_equ_N2O_AFO_Lan_*`
  - `Emi_CO2_Lan_Use_Flo_*`
  - these are baseline land-side flows, not technology mitigation
- Land mitigation by technology:
  - `Emi_Red_CO2_AFO_Lan_*`
  - `Emi_Red_equ_GHG_AFO_Lan`
  - these are technology-side land mitigation outputs
- Land technology area:
  - `Lan_Are_*`
  - these are cumulative active area, not annual newly converted area

## Land Sign Convention Rule
- In refs such as `prog/refs/hasegawa2025.R`, land-side figures often mix:
  - positive emission reduction terms
  - negative sequestration terms
- Important examples:
  - `Emi_Red_CO2_AFO_Lan_Pla`, `Emi_Red_CO2_AFO_Lan_Ref`, `Emi_Red_CO2_AFO_Lan_Pro`, `Emi_Red_CO2_AFO_Lan_Man`, `Emi_Red_CO2_AFO_Lan_Bch`
    - technology-side mitigation terms
    - these are typically handled as positive reduction values
  - `Emi_CO2_AFO_Aff`
    - afforestation / reforestation sink-side term
    - this is typically negative
  - `Car_Seq_Lan_Use_Soi_Car_Man`, `Car_Seq_Lan_Use_Soi_Car_Man_Cro`, `Car_Seq_Lan_Use_Soi_Car_Man_Gra`
    - soil carbon sequestration terms
    - these are typically negative
- Therefore, if a figure is meant to match the refs style of "emission reduction positive, sequestration negative", do not put only biochar / soil carbon on the negative side.
- In that case, include the other sink-side terms such as `Emi_CO2_AFO_Aff` as well.

## Land Interpretation Warning
- `Emi_Red_CO2_AFO_Lan_Pla` / `Emi_Red_CO2_AFO_Lan_Ref` and `Emi_CO2_AFO_Aff` are related but not identical representations.
- The former are technology mitigation variables.
- The latter is an afforestation sink flow.
- Refs sometimes place both in the same figure family, but conceptually they are different views.
- Do not blindly sum them together as if they were the same quantity.
- When making a new figure, decide first whether it is:
  - a technology mitigation figure, or
  - a land emissions / sequestration flow figure
- If both views are shown in one plot for comparability with refs, note internally that apparent double counting is possible if interpreted carelessly.

## Land Aggregation Rule
- The following aggregated land mitigation variables are valid and should be preferred when the plot is about grouped land mitigation:
  - `Emi_Red_CO2_AFO_Lan_Pro`
  - `Emi_Red_CO2_AFO_Lan_Res`
  - `Emi_Red_CO2_AFO_Lan_Man`
- These should be treated as grouped mitigation categories, not baseline emissions.
- For soil-carbon-side grouped sink terms under PLUM-style output, also check:
  - `Car_Seq_Lan_Use_Soi_Car_Man`
  - `Car_Seq_Lan_Use_Soi_Car_Man_Cro`
  - `Car_Seq_Lan_Use_Soi_Car_Man_Gra`

## Current Project Practice
- In `prog/analysis.R` and `prog/paperV1.R`, land area change should come from `AGMIP_all_scenario.gdx / LAND`, not from `Mergedcomparison.gdx`.
- For land-side mitigation potential figures based on AFOLUB:
  - default scenario is `SSP2_1100C_CACN_DAC_NoCC_No`
- For land-side actual mitigation between AFOLUB cases:
  - use `Emi_*` variables and calculate `No - LandInt`
- If a land-side figure is based on refs / Hasegawa-style reduction bars:
  - check whether the figure intends to show positive reduction plus negative sequestration
  - if so, include sink-side variables consistently rather than only one subset

## Important GDX Structure Notes
- `AGMIP_all_scenario.gdx`
  - Main symbol used so far: `AGMIP`
  - Observed dimensions:
    - `SCENARIO`
    - `cty`
    - `AgMIP_VAR`
    - `ITEM`
    - `AgMIP_UNIT`
    - `Y`
  - Example variables:
    - `CALI`
    - `EMIS`
    - `XPRP`
  - Example items:
    - `TOT`, `wht`, `ric`, `cgr`, `osd`, `rum`, `nrm`, `dry`
    - emissions source items such as `ENT`, `MMG`, `RCC`, `SFR`, `MAS`, `MGR`, `CRS`, `BCR`
- `IAMC_templatePlus.gdx`
  - Main symbol used so far: `IAMC_templatePlus`
  - Observed dimensions:
    - `Scenario`
    - `REMF`
    - `VEMF`
    - `YEMF`
  - Useful variables confirmed in current work:
    - `Red_Cos_AFO_Agr`
    - `Emi_Red_CH4_AFO_Agr`
    - `Emi_Red_N2O_AFO_Agr`
    - `Emi_Red_CO2_AFO_Lan`
    - `Emi_equ_CH4_AFO_Agr`
    - `Emi_equ_N2O_AFO_Agr`
    - `Agr_Pro_Cro_NonEne`
    - `Agr_Pro_Liv`
    - `Prc_Agr_Cor_Ind`
  - Note:
    - use these as original AFOLUB outputs
    - avoid building large composite indicators unless explicitly requested
- `Mergedcomparison.gdx`
  - Main symbols used so far:
    - `IAMCTemp`
    - `IAMCTempwoU`
    - `GHG`
    - `AREA`
  - `IAMCTemp` dimensions:
    - `scenario`
    - `region`
    - `variable`
    - `unit`
    - `year`
  - `IAMCTempwoU` dimensions:
    - `scenario`
    - `region`
    - `variable`
    - `year`
  - `GHG` dimensions:
    - `scenario`
    - `region`
    - `year`
    - `component`
    - `model`
  - `AREA` dimensions:
    - `scenario`
    - `region`
    - `year`
    - `land_code`
    - `model`
  - Useful variables confirmed in current work:
    - `Emi_CO2_AFO`
    - `Lan_Cov_Frs`
    - `Lan_Cov_Cro`
    - `Lan_Cov_Pst`
    - `Emi_CO2_Lan_Use_Flo_Pos_Emi`
    - `Emi_CO2_Lan_Use_Flo_Neg_Seq`
    - `Emi_CO2_AFO_Def`
    - `Emi_CO2_AFO_Aff`
  - Useful `GHG` components:
    - `Sink`
    - `Emissions`
    - `Net_emissions`
  - Useful `AREA` land codes:
    - `FRS`
    - `CL`
    - `PAS`
    - `PRMFRS`
    - `SECFRS`
    - and others if needed

## Scenario Notes
- Use these scenarios as the default comparison set:
  - Baseline: `SSP2_BaU_NoCC_No`
  - Mitigation with market feedback: `SSP2_1100C_CACN_DAC_NoCC_LandInt_global1`
- `SSP2_1100C_CACN_DAC_NoCC_No` is the Harmsen MACC case.
- Current default for GEER2026 exploratory figures is:
  - compare Baseline vs LandInt
  - do not include Harmsen comparison unless explicitly needed
- Important detail:
  - `IAMC_templatePlus.gdx` is mitigation-side and is not the clean default source for baseline comparisons
  - baseline-sensitive comparisons should usually be built from `Mergedcomparison.gdx` or `AGMIP_all_scenario.gdx`
  - `IAMC_templatePlus.gdx` does not contain a normal country-level `BaU` scenario in the way `AGMIP_all_scenario.gdx` does
  - when `IAMC_templatePlus.gdx` is used and a baseline-emissions level is needed, use the `Emi_*` variables from the mitigation scenario such as `SSP2_1100C_CACN_DAC_NoCC_No` or `SSP2_1100C_CACN_DAC_NoCC_LandInt_global1`
  - in this file family, `Emi_*` variables should be treated as baseline emissions associated with that mitigation case, not as residual emissions after reduction
  - do not assume `BaU` is available inside `IAMC_templatePlus.gdx`
- Trade scenarios such as `TBDECR` and `TBINCR` exist in some GDX files but are currently out of scope unless explicitly requested.

## Analysis Intent
- Do not prioritize simple variable-by-variable line plots by major region. Many of those already exist.
- Preferred additions are figures that help decide the paper direction:
  - country-level heterogeneity
  - concentration of mitigation across countries
  - country rankings under different original variables
  - scatter relationships using original variables or simple scenario changes
  - maps
  - land-use and LULUCF-focused views
- Main analytical emphasis should remain on ALPHA and AFOLUB, with Mergedcomparison used heavily for baseline and land-use/LULUCF interpretation.

## Indicator Construction Rule
- Do not create many new custom indicators by default.
- Preferred order:
  - original model variable
  - simple scenario difference such as `baseline - mitigation`
  - simple scenario change such as `mitigation - baseline`
  - share or rank only if needed to organize the figure
- Avoid introducing synthetic composite indices unless:
  - the user explicitly asks for them, or
  - they are clearly labeled as exploratory only
- If a composite or typology helper is used for sorting or grouping, keep the plotted axes on original variables whenever possible.

## Code Documentation Rule
- In `prog/*.R`, add concrete comments before major blocks.
- Comments should state:
  - purpose of the block
  - which GDX file is used
  - which symbol is used
  - which variable names are used
  - what difference or relationship is being plotted
- Example style:
  - `# Use Mergedcomparison.gdx / GHG / Net_emissions to compare baseline vs LandInt net LULUCF outcomes by country.`
  - `# Use IAMC_templatePlus.gdx / IAMC_templatePlus / Red_Cos_AFO_Agr as the AFOLUB cost variable for the mitigation scenario.`
- Do not leave only vague comments such as `# make plot`.
- For newer workflow scripts such as `prog/analysis.R`, shorter comments are acceptable.
  - Prefer brief block headers such as `# AGMIP`, `# AFOLUB full`, `# reshape`, `# plot`
  - Keep comments useful, but do not over-describe every line

## analysis.R Style Rule
- `prog/analysis.R` should be treated as the default interactive analysis template for GEER2026.
- Follow a structure similar to `Papers/ALPHApaper/AIMALPHAanalysis/prog/Visualization.R`:
  - library and theme setup
  - path and scenario settings
  - main data reads near the top
  - then repeated figure blocks
- Prefer defining the main GDX-derived objects early, for example:
  - `GEER_AGMIP`
  - `GEER_AFOLUB`
  - `GEER_MERGED`
- For GEER2026, do not fully expand large yearly AGMIP outputs unless necessary.
  - default to decadal years such as `2020, 2030, ..., 2100`
- After the initial setup and data reads, structure the script by figure group.
  - Each figure block should do its own small reshape and plot.
  - Avoid one large shared reshape section for the whole script.
  - The intended workflow is:
    - run the top setup and data-read part once
    - then re-run only the figure block you are editing
    - the block should recreate the plot without depending on a distant reshape section
  - keep yearly reads only for cases where the analysis truly needs them

## Plot Labeling Rule
- For exploratory scripts going forward, plot titles, subtitles, captions, and axis labels do not need extremely detailed variable tracing by default.
- Prefer concise labels that are easy to read in RStudio plots.
- Keep detailed source tracking mainly in:
  - code comments
  - object names
  - optional captions only when the distinction is important
- The earlier very long label style used in `2nd_check.R`, `3rd_check.R`, and parts of `4th_check.R` was useful for debugging, but it is no longer the default style.

## Implementation Notes for R Scripts
- Prefer partial reads from GDX using `gdxrrw::rgdx()` with filtered `uels`.
- Avoid full reads or wide `unique()` calls on large GDX tables unless absolutely necessary.
- `rgdx.param()` is convenient but can be too heavy on these files.
- For `IAMCTemp`, always filter both `variable` and `unit`; wrong unit combinations silently return zeros.
- When adding outputs, keep them under a dated subdirectory such as `output/0311/`.
- When joining sources, check that the resulting table is filtered back to `define/country.set`.

## Resource and Environment Warnings
- Large exploratory reads can fail with:
  - `Error: システム リソースが不足しているため、要求されたサービスを完了できません。 (os error 1450)`
  - timeouts when dumping full GDX metadata
- Common causes:
  - reading entire very large tables
  - calling `unique()` over full large columns
  - dumping full GDX contents to console
- Safer workflow:
  - restrict scenarios first
  - restrict years first
  - restrict countries using `define/country.set`
  - restrict variables and units first
  - test on a few countries before scaling to all countries
- In this environment, writing to some subdirectories may fail inside the shell sandbox even when files are visible.
  - If `Rscript` cannot write to `output/...` with permission denied, rerun with escalated permission.

## Current Output Produced By Scripts
- `prog/1st_check.R`
  - earlier exploratory figures mixing ALPHA, AFOLUB, and some Mergedcomparison support
- `prog/2nd_check.R`
  - current emphasis is Baseline vs LandInt country heterogeneity
  - current figures include:
    - mitigation potential map
    - concentration curve and top-N share comparison
    - mitigation vs cost scatter
    - forest gain vs mitigation scatter
    - sink vs net-emissions scatter
    - policy-attractiveness and net-emissions maps
    - typology quadrants
    - top-country mitigation portfolio
    - top-country metric heatmap
    - residual agricultural emissions
    - land-system change panels
    - AREA heatmap
    - GHG component comparison
    - top-country-by-indicator panels

## Recommended Workflow For Future Edits
- Read `doc/` files before adding new figures so the script supports the intended paper question.
- Check `define/AGMIP.set`, `define/IAMC.set`, and `define/country.set` before guessing variables or country coverage.
- Reuse the existing partial-read pattern in `prog/1st_check.R` and `prog/2nd_check.R`.
- If a variable returns all zeros, verify:
  - scenario name
  - unit string
  - whether the variable belongs to `IAMC_templatePlus`, `Mergedcomparison`, or `AGMIP`
- Prefer a few strong figures on country heterogeneity and concentration over many generic plots.
