# ============================================================================
# Step4_Operationalize.R
# Create binary autocracy indicator, log-transform GDP and population,
# create lagged treatment variables (2, 3, and 4 years)
# ============================================================================
rm(list = ls())

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# --- Load merged data from Step 2 ---
input_path <- "Data/cy_cssmean_vdem_merged.rds"
if (!file.exists(input_path)) stop(paste0("Run Step2 first. File not found: ", input_path))
data <- readRDS(input_path)
cat("Loaded:", input_path, "(", nrow(data), "rows )\n\n")

# --- Create binary autocracy indicator ---
# 1 if v2x_regime < 2 (closed or electoral autocracy), 0 otherwise
data$autocracy <- ifelse(!is.na(data$v2x_regime) & data$v2x_regime < 2, 1,
                         ifelse(!is.na(data$v2x_regime) & data$v2x_regime >= 2, 0, NA))

cat("=== Autocracy indicator ===\n")
cat("  Autocracy (1):", sum(data$autocracy == 1, na.rm = TRUE), "\n")
cat("  Democracy (0):", sum(data$autocracy == 0, na.rm = TRUE), "\n")
cat("  NA:", sum(is.na(data$autocracy)), "\n\n")

# --- Log-transform GDP and population ---
data$log_gdp <- log(data$e_gdppc + 1)
data$log_pop <- log(data$e_pop + 1)

# --- Create lagged variables (2, 3, and 4 years) ---
# Sort by country and year first
data <- data %>% arrange(vdem_code, year)

data <- data %>%
  group_by(vdem_code) %>%
  mutate(
    # Polyarchy lags
    v2x_polyarchy_lag2 = dplyr::lag(v2x_polyarchy, 2, order_by = year),
    v2x_polyarchy_lag3 = dplyr::lag(v2x_polyarchy, 3, order_by = year),
    v2x_polyarchy_lag4 = dplyr::lag(v2x_polyarchy, 4, order_by = year),
    # Autocracy lags
    autocracy_lag2 = dplyr::lag(autocracy, 2, order_by = year),
    autocracy_lag3 = dplyr::lag(autocracy, 3, order_by = year),
    autocracy_lag4 = dplyr::lag(autocracy, 4, order_by = year),
    # GDP lags
    log_gdp_lag2 = dplyr::lag(log_gdp, 2, order_by = year),
    log_gdp_lag3 = dplyr::lag(log_gdp, 3, order_by = year),
    log_gdp_lag4 = dplyr::lag(log_gdp, 4, order_by = year),
    # Population lags
    log_pop_lag2 = dplyr::lag(log_pop, 2, order_by = year),
    log_pop_lag3 = dplyr::lag(log_pop, 3, order_by = year),
    log_pop_lag4 = dplyr::lag(log_pop, 4, order_by = year)
  ) %>%
  ungroup()

cat("=== Lag variable coverage ===\n")
lag_vars <- c("v2x_polyarchy_lag2", "v2x_polyarchy_lag3", "v2x_polyarchy_lag4",
              "autocracy_lag2", "autocracy_lag3", "autocracy_lag4",
              "log_gdp_lag2", "log_gdp_lag3", "log_gdp_lag4",
              "log_pop_lag2", "log_pop_lag3", "log_pop_lag4")
for (v in lag_vars) {
  cat(sprintf("  %-25s non-NA: %d / %d\n", v, sum(!is.na(data[[v]])), nrow(data)))
}

cat("\n=== Summary of new variables ===\n")
print(summary(data[, c("autocracy", "log_gdp", "log_pop",
                        "v2x_polyarchy_lag2", "autocracy_lag2",
                        "log_gdp_lag2", "log_pop_lag2")]))

# --- Save ---
out_path <- "Data/cy_cssmean_operationalized.rds"
saveRDS(data, out_path)
cat("\nSaved to:", out_path, "\n")
cat("Done.\n")
