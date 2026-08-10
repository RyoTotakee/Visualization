# AGMIP all_scenario GDX cheatsheet

This note summarizes the parts of `AGMIP_all_scenario.gdx` that were repeatedly important in `GEER2026`.

Primary references:

- `Papers/GEER2026/define/AGMIP.set`
- `Papers/GEER2026/prog/paperV3.R`

## 1. Dataset structure

### `AGMIP`

Country-level data with dimensions equivalent to:

- `SCENARIO`
- `cty`
- `AgMIP_VAR`
- `ITEM`
- `AgMIP_UNIT`
- `Y`

Value field:

- `AGMIP`

Recommended renamed columns:

- `scenario`
- `region`
- `variable`
- `item`
- `unit`
- `year`
- `value`

### `AGMIP_agg`

Aggregate data with dimensions equivalent to:

- `SCENARIO`
- `region`
- `AgMIP_VAR`
- `ITEM`
- `AgMIP_UNIT`
- `Y`

Value field:

- `AGMIP_agg`

Use this for:

- `World`
- R5 or other aggregate regions already present in the GDX

## 2. High-value variable reminders

### Socioeconomic

- `POPT`
  - total population
  - unit often `million`
- `GDPT`
  - total GDP
  - in current GEER2026 work, prefer `bn USD 2017 PPP`
- `CTAX`
  - carbon tax
  - unit `USD/tCO2e`

### Food and production

- `PROD`
  - production
  - can appear in `1000 t` or `million kcal`
- `CONS`
  - domestic use
  - can appear in `1000 t` or `million kcal`
- `CALI`
  - calorie intake
  - unit `kcal/cap/d`
- `CALO`
  - calorie availability
- `NETT`
  - net trade
  - typically `1000 t`
- `XPRC`
  - consumer price
  - unit `USD/t`
- `XPRP`
  - producer or input price
  - unit `USD/t`

### Land and emissions

- `LAND`
  - land cover
  - unit `1000 ha`
- `EMIS`
  - total GHG emissions
  - unit `MtCO2e`
- `RedCos`
  - mitigation cost
  - in current scripts this is treated as `USD`, then rescaled explicitly

## 3. Important item codes

### Product items

- `RIC`: rice
- `WHT`: wheat
- `CGR`: coarse grains
- `OSD`: oilseeds
- `SGC`: sugar crops
- `VFN`: vegetables, fruits, nuts
- `OCR`: other crops
- `RUM`: ruminant meats
- `NRM`: non-ruminant meats
- `DRY`: dairy

### Aggregates

- `CRP`: all crops
- `LSP`: livestock products
- `AGR`: all agricultural products
- `TOT`: total economy or total land/population/GDP context

### Land items

- `CRP`: cropland including energy crops
- `ECP`: energy crops
- `GRS`: grassland
- `FOR`: managed and primary forest
- `ONV`: other natural land
- `NLD`: non-arable land
- `AGR`: cropland + grassland
- `NAT`: natural land = forest + other natural land

### Agricultural emission source items

- `ENT`: enteric fermentation
- `MMG`: manure management
- `RCC`: rice cultivation
- `SFR`: synthetic fertilizers
- `MAS`: manure applied to soils
- `MGR`: manure left on pasture
- `CRS`: crop residues
- `ORS`: cultivation of organic soils
- `BSV`: burning - savanna
- `BCR`: burning - crop residues

Useful current grouping in GEER2026:

- `Enteric fermentation` = `ENT`
- `Manure management` = `MMG`
- `Rice cultivation` = `RCC`
- `Managed soils` = `SFR + MAS + MGR + CRS + ORS + BSV + BCR`

## 4. The main traps

### 4.1 Item casing

Raw `ITEM` values are inconsistent.

Examples seen in practice:

- `tot`
- `ent`
- `mmg`
- `rcc`
- `WHT`
- `RIC`
- `CRP`
- `AGR`

Normalize with:

```r
mutate(item = toupper(item))
```

before doing any joins or recodes.

### 4.2 One item code can mean different things under different variables

Examples:

- `AGR`
  - under `PROD`, `CONS`, `CALI`, `EMIS`: agriculture aggregate
  - under `LAND`: cropland + grassland aggregate
- `TOT`
  - under `POPT`, `GDPT`, `LAND`: total economy or total land base

Always filter with:

- `variable`
- `item`
- `unit`

as a set.

### 4.3 Unit switching is common

The same variable may exist in multiple units.

Especially important:

- `PROD`
  - `1000 t`
  - `million kcal`
- `CONS`
  - `1000 t`
  - `million kcal`
- `GDPT`
  - multiple unit variants may exist across workflows

Do not assume one canonical unit without checking the exact `unit` value.

### 4.4 2015 is easy to drop by accident

If you read only:

```r
seq(2020, 2100, 10)
```

you will not be able to compare `2100` with `2015`.

In GEER2026, when 2015 comparisons are needed, use something like:

```r
analysis_years <- sort(unique(c(2015L, seq(2020, 2100, 10))))
```

### 4.5 `LAND(CRP)` includes energy crops

If you want cropland excluding energy crops:

```r
cropland_non_energy = LAND(CRP) - LAND(ECP)
```

This is already used in the current GEER2026 scripts.

## 5. Current GEER2026 usage patterns

### Use `AGMIP` for country plots

Examples:

- top-country `CALI`
- top-country `XPRC`
- top-country `PROD`
- country-level `EMIS`

### Use `AGMIP_agg` for `World` or R5 panels

Examples:

- `World` energy crop demand
- `World` or R5 aggregate emissions

### Typical filters used in `paperV3.R`

Examples:

```r
variable == "CALI" & item %in% c("AGR", "CRP", "LSP") & unit == "kcal/cap/d"
variable == "PROD" & item %in% c("AGR", "CRP", "LSP", "WHT", "RIC", ...) & unit == "million kcal"
variable == "LAND" & item %in% c("TOT", "CRP", "ECP", "GRS", "ONV", "NLD", "FOR") & unit == "1000 ha"
variable == "EMIS" & item %in% c("ENT", "MMG", "RCC", "SFR", "MAS", "MGR", "CRS", "ORS", "BSV", "BCR") & unit == "MtCO2e"
variable == "RedCos" & item == "AGR" & unit == "USD"
variable == "XPRC" & item %in% c("CRP", "LSP") & unit == "USD/t"
```

## 6. Derived indicators used in practice

### GDP per capita

If:

- `GDPT` is `bn USD 2017 PPP`
- `POPT` is `million`

then:

\[
GDPT / POPT
\]

gives `1000 USD per person`.

### Self-sufficiency

When using energy units:

\[
100 \times \frac{PROD(AGR)}{CONS(AGR)}
\]

with both in `million kcal`.

### Land-use change share

\[
100 \times \frac{\Delta LAND(item)}{LAND(TOT)_{BaU}}
\]

with land in `1000 ha`.

## 7. Recommended validation checks

Whenever you add a new plot or metric:

1. Check that `item` has been uppercased.
2. Check the exact `unit`.
3. Confirm whether the required baseline year is loaded.
4. If aggregating to `World` or R5, prefer `AGMIP_agg`.
5. For country plots, confirm the region is in `country.set`.
6. If values look like all zeros, check:
   - wrong `item`
   - wrong `unit`
   - missing year in the read
   - aggregate vs country table confusion

## 8. Most reusable snippets

### Country-level read

```r
GEER_AGMIP <- read_agmip(
  variables = c("CALI", "XPRC", "PROD", "CONS", "LAND", "POPT", "GDPT", "EMIS", "RedCos"),
  items = c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS", "ECP", "ONV", "NLD", "tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "ors", "bsv", "bcr", "WHT", "RIC", "CGR", "OSD", "SGC", "VFN", "OCR", "RUM", "NRM", "DRY"),
  units = c("kcal/cap/d", "USD/t", "1000 ha", "million", "million kcal", "1000 t", "bn USD 2017 PPP", "MtCO2e", "USD"),
  regions = regions_for_read,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)
```

### Aggregate read

```r
GEER_AGMIP_AGG <- rgdx.param(file.path(data_dir, "AGMIP_all_scenario.gdx"), "AGMIP_agg") %>%
  transmute(
    scenario = as.character(SCENARIO),
    region = as.character(region),
    variable = as.character(AgMIP_VAR),
    item = toupper(as.character(ITEM)),
    unit = as.character(AgMIP_UNIT),
    year = as.integer(as.character(Y)),
    value = as.numeric(AGMIP_agg)
  )
```

## 9. If something looks wrong

Start with these checks in order:

1. `item` uppercase?
2. exact `unit` correct?
3. `AGMIP` vs `AGMIP_agg` confusion?
4. year actually read?
5. did the requested `item` exist in `AGMIP.set`?
6. are you using a land aggregate that double counts `ECP` inside `CRP`?

