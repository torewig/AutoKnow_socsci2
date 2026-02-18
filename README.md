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

The pipeline consists of eight scripts, designed to be run in order. All scripts should be executed from the repository root (the parent of `Data/`).

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

### Step 6: Descriptive Figures and Tables (`Step6_Descriptives.R`)

Generates a set of figures and LaTeX tables exploring the relationship between political regime characteristics and social science output.

**Figures** (saved to `Figures/`):

| Figure | Description |
|--------|-------------|
| `fig1_polyarchy_cssmean_scatter.png` | Scatter plot of polyarchy vs CSSmean with LOESS smoother. |
| `fig2_cssmean_by_regime_boxplot.png` | Box plots of CSSmean by V-DEM regime type. |
| `fig3_cssmean_by_regime_violin.png` | Violin plots of CSSmean by regime type with quartile lines. |
| `fig4_binned_polyarchy_cssmean.png` | Binned scatter: mean CSSmean by polyarchy decile with 95% CIs. |
| `fig5_cssmean_trends_by_regime.png` | Time series of mean CSSmean by regime type (1990--2019). |
| `fig6_polyarchy_cssmean_by_regime.png` | Scatter of polyarchy vs CSSmean, colored by regime type with separate linear fits. |
| `fig7_heatmap_polyarchy_decade.png` | Heatmap of mean CSSmean by polyarchy quartile and decade. |

**Tables** (saved to `Tables/`):

| Table | Description |
|-------|-------------|
| `Desc_Regime.tex` | Descriptive statistics (N, countries, mean, SD, min, median, max) by regime type. |
| `Desc_Polyarchy_Quartile.tex` | Descriptive statistics by polyarchy quartile, including share classified as autocracy. |
| `Correlation_Matrix.tex` | Pairwise correlation matrix for CSSmean, polyarchy, GDP, population, and autocracy. |

### Step 7: Synthetic Control Analysis (`Step7_SynthControl.R`)

Identifies countries that transitioned to autocracy with sufficient pre- and post-treatment data, then runs synthetic control analysis using the `Synth` package.

**Part 1 -- Candidate identification:**
- Detects all transitions where `autocracy` changes from 0 to 1.
- Filters for cases with at least 4 pre-treatment and 3 post-treatment years of non-missing `cssmean`.
- Keeps the first transition per country. Found 21 candidates.

**Part 2 -- Synthetic control estimation:**
- Donor pool: 58 countries that remain democratic throughout the panel.
- Requires donors and treated unit to have complete pre-period `cssmean`.
- 5 cases completed successfully:

| Country | Treatment year | Pre-period years | Pre-RMSPE | Top donor |
|---------|---------------|-----------------|-----------|-----------|
| India | 2017 | 15 | 2.82 | Japan (0.58) |
| Kenya | 2007 | 15 | 4.54 | Barbados (0.57) |
| Philippines | 2004 | 13 | 5.67 | Barbados (0.36) |
| Thailand | 2006 | 15 | 5.92 | Barbados (0.31) |
| Turkey | 2013 | 15 | 1.88 | Japan (0.37) |

**Part 3 -- Figures** (saved to `Figures/`):

| Figure | Description |
|--------|-------------|
| `fig_synth_[country].png` | Individual actual vs synthetic CSSmean plots (5 countries). |
| `fig_synth_gaps_all.png` | Overlay of gap (actual - synthetic) for all cases. |
| `fig_synth_avg_gap.png` | Average gap across all cases with 95% CI. |
| `fig_synth_facet_all.png` | Faceted panel showing all 5 cases side by side. |

### Step 8: Panel Matching (`Step8_PanelMatch.R`)

Estimates the average treatment effect on the treated (ATT) of autocracy on CSSmean using the `PanelMatch` package (v3 API) with Mahalanobis refinement on log GDP per capita and log population.

**Part 1 -- Data preparation:**
- Filters to 40 countries with consecutive year observations (847 country-years).
- Creates a `PanelData` object (PanelMatch v3 API) with unit, time, treatment, and outcome identifiers.
- 281 autocracy observations, 566 democracy observations.

**Part 2 -- ATT estimation:**
Three specifications with matching lags of 1, 2, and 3 years, each estimating effects at leads 0--5 (years after treatment onset). Bootstrap standard errors with 1,000 iterations.

| Specification | t+0 | t+2 | Placebo passes? |
|---|---|---|---|
| Lag 1 | 0.69 (n.s.) | 1.59 (n.s.) | -- |
| Lag 2 | -3.34\* | -3.68\* | Yes (CI includes zero) |
| Lag 3 | -3.24\* | -3.68\* | Yes |

The preferred specification (lag 2) shows a statistically significant negative effect of autocracy on social science output at t+0 and t+2, with the 95% bootstrap CI excluding zero.

**Part 3 -- Placebo test:**
- Pre-treatment placebo estimate: -2.71, 95% CI [-8.36, 2.94].
- The CI includes zero, supporting the parallel trends assumption.

**Part 4 -- Figures** (saved to `Figures/`):

| Figure | Description |
|--------|-------------|
| `fig_panelmatch_att.png` | ATT for all three lag specifications with 95% CIs. |
| `fig_panelmatch_att_lag2.png` | Preferred specification (lag 2) with CI ribbon. |
| `fig_panelmatch_prepost.png` | Pre-treatment placebo and post-treatment ATT on a single timeline. |

**Part 5 -- LaTeX table:** `Tables/PanelMatch_ATT.tex` -- ATT estimates for all three specifications with 95% bootstrap CIs.

## Output

All output files from Steps 5, 6, 7, and 8:

| File | Description |
|------|-------------|
| `Tables/TWFE_Polyarchy.tex` | LaTeX table: TWFE models with polyarchy as treatment (lags 2, 3, 4). |
| `Tables/TWFE_Autocracy.tex` | LaTeX table: TWFE models with binary autocracy as treatment (lags 2, 3, 4). |
| `Tables/Desc_Regime.tex` | LaTeX table: CSSmean descriptive statistics by regime type. |
| `Tables/Desc_Polyarchy_Quartile.tex` | LaTeX table: CSSmean descriptive statistics by polyarchy quartile. |
| `Tables/Correlation_Matrix.tex` | LaTeX table: pairwise correlation matrix. |
| `Figures/fig1-fig7` | PNG figures from Step 6 (see above for descriptions). |
| `Figures/fig_synth_*.png` | Synthetic control figures from Step 7 (5 country plots + 3 summary plots). |
| `Tables/PanelMatch_ATT.tex` | LaTeX table: PanelMatch ATT estimates for lags 1, 2, 3 with bootstrap CIs. |
| `Figures/fig_panelmatch_att.png` | PanelMatch ATT for all three lag specifications. |
| `Figures/fig_panelmatch_att_lag2.png` | PanelMatch preferred specification (lag 2) with CI ribbon. |
| `Figures/fig_panelmatch_prepost.png` | PanelMatch pre-treatment placebo and post-treatment ATT. |

## Exploratory Scripts

Earlier data-creation scripts and additional exploratory analyses (synthetic control, panel matching, pre/post plots) have been moved to the `Exploratory/` folder. See `Exploratory/README.md` for details.

## Requirements

- R (tested with 4.5.1)
- R packages: `countrycode`, `dplyr`, `fixest`, `ggplot2`, `tidyr`, `scales`, `Synth`, `PanelMatch`
