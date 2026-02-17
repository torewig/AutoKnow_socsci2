# AutoKnow SocSci2: CSSmean Analysis Pipeline

This repository contains the analysis pipeline for examining the relationship between social science output (CSSmean) and political regime characteristics, using V-DEM (Varieties of Democracy) data.

## Data

All data files live in the `Data/` folder:

| File | Description |
|------|-------------|
| `cntryyear_avgs.csv` | Country-year averages of CSS (Composite Social Science) scores. 3,598 observations across 177 countries (1970--2019). Key variable: `cssmean`. |
| `V-Dem-CY-Full+Others-v15.rds` | V-DEM v15 country-year dataset (27,913 rows, 4,607 variables). |
| `cy_step1_mapped.rds` | Intermediate output from Step 1: country-year data with V-DEM codes mapped. |
| `cy_cssmean_vdem_merged.rds` | Output from Step 2: country-year data merged with V-DEM variables. Also saved as `.csv`. |
| `cy_cssmean_operationalized.rds` | Output from Step 4: merged data with autocracy indicator and lagged variables. |

## Analysis Pipeline

The pipeline consists of five scripts, designed to be run in order. All scripts should be executed from the repository root (the parent of `Data/`).

### Step 1: Load and Map V-DEM Codes (`Step1_LoadAndMap.R`)

- Reads `Data/cntryyear_avgs.csv`.
- Maps country names to V-DEM numeric country codes using the `countrycode` R package (destination `"vdem"`).
- Reports mapping success and lists any unmapped countries.
- Saves intermediate result to `Data/cy_step1_mapped.rds`.

**Result:** All 3,598 rows successfully mapped to V-DEM codes.

### Step 2: Merge with V-DEM (`Step2_MergeVDEM.R`)

- Loads the mapped data from Step 1 and the V-DEM v15 dataset.
- Subsets V-DEM to four variables:
  - `v2x_polyarchy` -- Electoral democracy index (0--1).
  - `e_gdppc` -- GDP per capita.
  - `v2x_regime` -- Regime type (0 = closed autocracy, 1 = electoral autocracy, 2 = electoral democracy, 3 = liberal democracy).
  - `e_pop` -- Population.
- Merges on `vdem_code` = `country_id` and `year` (left join, preserving all CSS rows).
- Saves merged data to `Data/cy_cssmean_vdem_merged.rds` and `Data/cy_cssmean_vdem_merged.csv`.

**Result:** 3,585 of 3,598 rows matched V-DEM data (99.6%).

### Step 3: Descriptive Analysis of CSSmean (`Step3_Analyze_cssmean.R`)

- Produces descriptive statistics for `cssmean`: mean, SD, range, country count, year range.
- Breaks down `cssmean` by V-DEM regime type.
- Reports correlations of `cssmean` with `v2x_polyarchy`, GDP, and population.
- Lists top/bottom 10 countries by average `cssmean`.
- Tabulates year-level averages of `cssmean` and `v2x_polyarchy`.

### Step 4: Operationalize Variables (`Step4_Operationalize.R`)

- Creates a binary `autocracy` indicator: 1 if `v2x_regime < 2`, 0 otherwise.
- Log-transforms GDP per capita (`log_gdp`) and population (`log_pop`).
- Creates 2-, 3-, and 4-year lagged versions of all treatment and control variables:
  - `v2x_polyarchy_lag2/3/4`
  - `autocracy_lag2/3/4`
  - `log_gdp_lag2/3/4`
  - `log_pop_lag2/3/4`
- Saves to `Data/cy_cssmean_operationalized.rds`.

**Result:** 1,472 autocracy observations, 2,113 democracy observations, 13 NA.

### Step 5: TWFE Regressions (`Step5_TWFE_Regressions.R`)

Estimates two-way fixed effects (country + year) regressions using `fixest::feols` with heteroskedasticity-robust standard errors. Dependent variable is `cssmean` in all models. Controls are lagged log GDP per capita and lagged log population.

**Panel A -- Polyarchy as treatment:**

| | Lag 2 | Lag 3 | Lag 4 |
|---|---|---|---|
| Polyarchy coeff. | -2.03 | -2.99\* | -2.62\* |
| N | 3,232 | 3,066 | 2,904 |
| Adj. R-sq | 0.387 | 0.407 | 0.408 |

**Panel B -- Autocracy (binary) as treatment:**

| | Lag 2 | Lag 3 | Lag 4 |
|---|---|---|---|
| Autocracy coeff. | 1.07\* | 1.17\*\* | 1.41\*\* |
| N | 3,232 | 3,066 | 2,904 |
| Adj. R-sq | 0.388 | 0.407 | 0.410 |

LaTeX-ready tables are saved to `Tables/TWFE_Polyarchy.tex` and `Tables/TWFE_Autocracy.tex`.

## Output

| File | Description |
|------|-------------|
| `Tables/TWFE_Polyarchy.tex` | LaTeX table: TWFE models with polyarchy as treatment (lags 2, 3, 4). |
| `Tables/TWFE_Autocracy.tex` | LaTeX table: TWFE models with binary autocracy as treatment (lags 2, 3, 4). |

## Exploratory Scripts

Earlier data-creation scripts and additional exploratory analyses (synthetic control, panel matching, pre/post plots) have been moved to the `Exploratory/` folder. See `Exploratory/README.md` for details.

## Requirements

- R (tested with 4.5.1)
- R packages: `countrycode`, `dplyr`, `fixest`
