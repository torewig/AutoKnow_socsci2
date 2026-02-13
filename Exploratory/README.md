# Exploratory Scripts

This folder contains scripts for exploratory and causal analysis of CSSmean and autocracy transitions.

## Script Descriptions

- **TWFE_Autocracy_CSSmean.R**
  - Estimates two-way fixed effects (country and year) models of `cssmean` as the outcome, with lagged `autocracy`, GDP, and population as predictors. Outputs model summaries and LaTeX tables for 2-year and 3-year lags.

- **Explore_SynthControl_Candidates.R**
  - Scans the panel data to identify countries and years with a clear transition to autocracy and sufficient pre/post data for synthetic control analysis. Lists suitable cases for further study.

- **Synth_Turkey_CSSmean.R**
  - Performs a synthetic control analysis for Turkey, using `cssmean` as the outcome and the transition to autocracy in 2013 as the treatment. Plots actual vs. synthetic Turkey and highlights the treatment year.
