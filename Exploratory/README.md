# Exploratory Scripts

This folder contains earlier data-creation scripts and exploratory/causal analyses of CSSmean and autocracy transitions. These scripts are not part of the main analysis pipeline (see root-level `README.md`).

## Earlier Data-Creation Scripts

- **Datacreation.R**
  - Reads country-level averages (`Data/cntry_avgs.csv`), maps V-DEM country codes using `countrycode`, and produces a ranked bar plot of CSSmean by country.

- **Datacreation2.R**
  - Loads country-year averages (`Data/cntryyear_avgs.csv`), maps V-DEM codes, merges with V-DEM v15 for `v2x_polyarchy`, `e_gdppc`, `v2x_regime`, and `e_pop`. Saves `Data/cy_merged_final.rds`.

- **Operationalization.R**
  - Loads `Data/cy_merged_final.rds`, creates a binary `autocracy` indicator (`v2x_regime < 2`), log-transforms GDP and population. Saves `Data/cy_operationalized.rds`.

## Exploratory and Causal Analyses

- **TWFE_Autocracy_CSSmean.R**
  - Estimates two-way fixed effects (country and year) models of `cssmean` as the outcome, with lagged `autocracy`, GDP, and population as predictors. Outputs model summaries and LaTeX tables for 2-year and 3-year lags.

- **Explore_SynthControl_Candidates.R**
  - Scans the panel data to identify countries and years with a clear transition to autocracy and sufficient pre/post data for synthetic control analysis. Lists suitable cases for further study.

- **Synth_Turkey_CSSmean.R**
  - Performs a synthetic control analysis for Turkey, using `cssmean` as the outcome and the transition to autocracy in 2013 as the treatment. Plots actual vs. synthetic Turkey and highlights the treatment year.

- **PanelMatch_CSSmean_Autocracy.R**
  - Runs panel matching (Mahalanobis refinement) to estimate the ATT of autocracy on CSSmean with pre-treatment placebo tests and post-treatment lead effects.

- **PrePostPlot.R**
  - Plots average CSSmean in a 5-year window before and after transitions to autocracy across all countries that experienced such a transition.
