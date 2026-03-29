# Repository Guidelines

## Project Structure & Module Organization
- `prog/` holds the R analysis/visualization scripts (e.g., `AFOLUBdata.R`, `IAMCtempPlus.R`, `future_analysis.R`).
- `data/` contains input data files such as `.gdx` bundles referenced by the scripts.
- `output/` is the default target for generated plots; additional dated outputs live in `output_0122/` and `output_0127/`.

## Build, Test, and Development Commands
- Run a script from the repo root with R (adjust the file name as needed):
  - `Rscript prog/AFOLUBdata.R`
  - `Rscript prog/IAMCtempPlus.R`
  - `Rscript prog/future_analysis.R`
- These scripts read from `data/` and write plots to `output/` (and subfolders defined in each script).

## Coding Style & Naming Conventions
- R scripts use 2-space indentation and `<-` assignment; keep that style consistent.
- Prefer `snake_case` for file and directory names, and keep variable names descriptive (e.g., `region_list`, `variable_list`).
- Plot outputs are named by sanitized variable strings; follow the existing safe-name pattern when adding new outputs.

## Testing Guidelines
- No automated tests are present in this repository.
- If you add tests, document the framework and a `Rscript` command to run them here.

## Commit & Pull Request Guidelines
- Commit history uses short, imperative messages (e.g., “Add all visualization scripts”).
- Keep commits focused on a single change set; include a brief description of outputs generated when relevant.
- PRs should summarize the scripts changed, data dependencies (new `.gdx` files), and example output paths or screenshots of key plots.

## Data & Configuration Notes
- Scripts assume relative paths like `../data` and `../output` from `prog/`; keep that layout intact.
- Ensure required R packages are installed (`dplyr`, `ggplot2`, `stringr`, `gdxrrw`).
