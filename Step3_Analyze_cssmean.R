# ============================================================================
# Step3_Analyze_cssmean.R
# Descriptive analysis of cssmean in the merged dataset
# ============================================================================
rm(list = ls())

# --- Load merged data ---
data_path <- "Data/cy_cssmean_vdem_merged.rds"
if (!file.exists(data_path)) stop(paste0("Run Step2_MergeVDEM.R first. File not found: ", data_path))
data <- readRDS(data_path)
cat("Loaded:", data_path, "(", nrow(data), "rows )\n\n")

# ============================================================================
# 1. Descriptive statistics for cssmean
# ============================================================================
cat("============================================================\n")
cat("1. DESCRIPTIVE STATISTICS FOR cssmean\n")
cat("============================================================\n")
print(summary(data$cssmean))
cat("  Std. dev:", sd(data$cssmean, na.rm = TRUE), "\n")
cat("  N obs:", sum(!is.na(data$cssmean)), "\n")
cat("  N countries:", length(unique(data$country[!is.na(data$cssmean)])), "\n")
cat("  Year range:", min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE), "\n\n")

# ============================================================================
# 2. cssmean by V-DEM regime type (v2x_regime)
# ============================================================================
cat("============================================================\n")
cat("2. cssmean BY REGIME TYPE (v2x_regime)\n")
cat("   0=Closed autocracy, 1=Electoral autocracy,\n")
cat("   2=Electoral democracy, 3=Liberal democracy\n")
cat("============================================================\n")
regime_labels <- c("0: Closed autocracy", "1: Electoral autocracy",
                   "2: Electoral democracy", "3: Liberal democracy")
for (r in 0:3) {
  sub <- data$cssmean[data$v2x_regime == r & !is.na(data$v2x_regime)]
  if (length(sub) > 0) {
    cat(regime_labels[r + 1], "\n")
    cat("  N:", length(sub), " Mean:", round(mean(sub, na.rm = TRUE), 2),
        " SD:", round(sd(sub, na.rm = TRUE), 2),
        " Min:", round(min(sub, na.rm = TRUE), 2),
        " Max:", round(max(sub, na.rm = TRUE), 2), "\n")
  }
}
cat("\n")

# ============================================================================
# 3. Correlation: cssmean with v2x_polyarchy and GDP
# ============================================================================
cat("============================================================\n")
cat("3. CORRELATIONS WITH cssmean\n")
cat("============================================================\n")
complete_poly <- complete.cases(data$cssmean, data$v2x_polyarchy)
cat("cssmean ~ v2x_polyarchy: r =",
    round(cor(data$cssmean[complete_poly], data$v2x_polyarchy[complete_poly]), 3),
    " (N =", sum(complete_poly), ")\n")

complete_gdp <- complete.cases(data$cssmean, data$e_gdppc)
if (sum(complete_gdp) > 2) {
  cat("cssmean ~ e_gdppc:       r =",
      round(cor(data$cssmean[complete_gdp], data$e_gdppc[complete_gdp]), 3),
      " (N =", sum(complete_gdp), ")\n")
  cat("cssmean ~ log(e_gdppc):  r =",
      round(cor(data$cssmean[complete_gdp], log(data$e_gdppc[complete_gdp] + 1)), 3),
      " (N =", sum(complete_gdp), ")\n")
}

complete_pop <- complete.cases(data$cssmean, data$e_pop)
if (sum(complete_pop) > 2) {
  cat("cssmean ~ log(e_pop):    r =",
      round(cor(data$cssmean[complete_pop], log(data$e_pop[complete_pop] + 1)), 3),
      " (N =", sum(complete_pop), ")\n")
}
cat("\n")

# ============================================================================
# 4. Top and bottom countries by average cssmean
# ============================================================================
cat("============================================================\n")
cat("4. TOP/BOTTOM COUNTRIES BY AVERAGE cssmean\n")
cat("============================================================\n")
country_means <- aggregate(cssmean ~ country, data = data, FUN = mean, na.rm = TRUE)
country_means <- country_means[order(-country_means$cssmean), ]

cat("Top 10 countries (highest cssmean):\n")
print(head(country_means, 10), row.names = FALSE)
cat("\nBottom 10 countries (lowest cssmean):\n")
print(tail(country_means, 10), row.names = FALSE)
cat("\n")

# ============================================================================
# 5. Year-level averages of cssmean
# ============================================================================
cat("============================================================\n")
cat("5. YEAR-LEVEL AVERAGES OF cssmean\n")
cat("============================================================\n")
year_means <- aggregate(cbind(cssmean, v2x_polyarchy) ~ year, data = data, FUN = mean, na.rm = TRUE)
year_means <- year_means[order(year_means$year), ]
cat("Year  | cssmean_avg | v2x_polyarchy_avg | N_obs\n")
cat("------|-------------|-------------------|------\n")
for (i in seq_len(nrow(year_means))) {
  n_yr <- sum(data$year == year_means$year[i] & !is.na(data$cssmean))
  cat(sprintf("%4d  |   %6.2f    |      %5.3f        |  %d\n",
              year_means$year[i], year_means$cssmean[i],
              year_means$v2x_polyarchy[i], n_yr))
}

cat("\n=== Analysis complete ===\n")
