# Repository Guidelines

## Project Structure & Module Organization
The repository is organized by analysis domains at the root: `AFOLUB_analysis/`, `ALPHA_analysis/`, `BIO_analysis/`, `PLUM_analysis/`, `PREDICTS_analysis/`, and `Papers/`. Each analysis subproject typically contains:
- `prog/` for R scripts (main entrypoints like `Visualization.R`, `Scenario_Compare.R`).
- `data/` for local input datasets (not versioned).
- `output/` or `output_*` for generated figures/tables (not versioned).
- `tool/` for helper scripts (when present).

Example entrypoint: `Papers/ALPHApaper/ALPHApaper_analysis/prog/Visualization.R`.

## Build, Test, and Development Commands
There is no centralized build system; run scripts per subproject.
- `Rscript prog/Visualization.R` runs a specific visualization script from the subproject root.
- Open the `.R` file in RStudio and run it interactively.
- Where provided, `bash prog/run.sh` executes a scripted pipeline.

Outputs are typically written to `output/` or `plot/` directories.

## Coding Style & Naming Conventions
- R scripts use `<-` for assignment and 2-space indentation; follow the local style in each `prog/` file.
- Prefer descriptive filenames that match the analysis task (e.g., `IAMCtemp.R`, `CScompare.R`).
- Keep object names readable and consistent with existing scripts (often snake_case or domain-specific abbreviations).
- For interactive analysis scripts, prefer this structure:
  - settings and paths first
  - main data reads near the top
  - then figure blocks that each do their own small reshape and plot
- Avoid one large global reshape section when the script is mainly for exploratory plotting.
- The goal is that, once data are loaded, a user can re-run a single figure block in RStudio and regenerate that plot directly.

## Testing Guidelines
No automated tests are defined at the repository root. Validation is manual:
- Run the relevant script and inspect generated figures/tables in `output*/` or `plot/`.
- If you add tests or checks, document how to run them in the subproject README.

## Data & Configuration
- Inputs live under `data/` directories and are ignored by git (`.gitignore` excludes `data/`, `output/`, `output*/`, `plot/`).
- Do not commit raw or generated datasets; document required external sources in the nearest README.

## Commit & Pull Request Guidelines
- Commit history uses short, imperative, sentence-case messages (e.g., "Add all visualization scripts"). Keep messages concise and task-focused.
- PRs should include a brief summary, list of scripts run, and sample figures if outputs change. Note any new data requirements and avoid committing ignored data.
