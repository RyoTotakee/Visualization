---
name: agmip-all-scenario-gdx
description: Use when working with Papers/GEER2026/data/0316/AGMIP_all_scenario.gdx or AGMIP_agg and you need the correct read pattern, item/unit rules, variable meanings, or common pitfalls for GEER2026 analysis scripts.
---

# AGMIP all_scenario GDX

Use this skill when reading or interpreting `AGMIP_all_scenario.gdx` in `Papers/GEER2026`.

Read [references/agmip-cheatsheet.md](references/agmip-cheatsheet.md) for the variable, item, and unit summary before adding new plots or summaries.

## Default workflow

1. Confirm whether you need country-level data or aggregate data.
   - `AGMIP` = country-level (`cty`)
   - `AGMIP_agg` = aggregate (`region`), used for `World` and R5-style summaries
2. Read only the variables, items, units, regions, years, and scenarios you need.
3. Normalize `ITEM` to upper case immediately after reading.
4. Filter by exact `variable + item + unit` combinations before reshaping.
5. If you derive rates or ratios, verify the denominator unit and whether it is a country total or a sector subtotal.

## Recommended read pattern

For `AGMIP`, prefer the same pattern used in `paperV3.R`:

```r
read_agmip <- function(variables, items, units, regions, years, scenarios) {
  req <- list(
    name = "AGMIP",
    form = "full",
    uels = list(scenarios, regions, variables, items, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
    mutate(
      item = toupper(item),
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    group_by(scenario, region, variable, item, unit, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}
```

For `AGMIP_agg`, `rgdx.param()` is usually enough, then rename:

- `SCENARIO -> scenario`
- `region -> region`
- `AgMIP_VAR -> variable`
- `ITEM -> item`
- `AgMIP_UNIT -> unit`
- `Y -> year`
- `AGMIP_agg -> value`

## Non-obvious rules

- `ITEM` casing is mixed in the raw GDX. Normalize with `toupper(item)`.
- Do not trust an `ITEM` code without also checking `variable` and `unit`.
- `TOT` and `tot` can both appear in raw reads. After normalization, both become `TOT`.
- `AGMIP.set` is the source of truth for code meanings, but not every code is meaningful for every variable.
- `World` and R5 values should generally come from `AGMIP_agg`, not by summing country `AGMIP` unless you intentionally want a custom aggregation.
- If you need 2015, add it explicitly to the year vector. A decadal sequence starting at 2020 will silently omit 2015.

## Common pitfalls

- Reading only `2020:2100 by 10` and then later trying to compare with `2015`.
- Mixing `PROD` in `1000 t` and `PROD` in `million kcal`.
- Mixing `GDPT` units; prefer `bn USD 2017 PPP` when matching current GEER2026 scripts.
- Forgetting that `LAND(CRP)` includes `ECP`; if you want cropland excluding energy crops, compute `CRP - ECP`.
- Using `RedCos` as if it were already in million or billion USD; check the unit and rescale explicitly.

## What to read next

- For code and unit/item examples:
  - [references/agmip-cheatsheet.md](references/agmip-cheatsheet.md)
- For the canonical current implementation:
  - `Papers/GEER2026/prog/paperV3.R`
- For raw item and variable definitions:
  - `Papers/GEER2026/define/AGMIP.set`
