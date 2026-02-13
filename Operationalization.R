# ============================================================================
# Operationalization.R
# Load merged data, log-transform variables, create autocracy indicator, save
# ============================================================================

# Load merged data
input_path <- "Data/cy_merged_final.rds"
if (!file.exists(input_path)) {
  stop(paste0("File not found: ", input_path))
}
data <- readRDS(input_path)
cat("Loaded data from", input_path, "\n")

# Log-transform GDP and Population (add small constant to avoid log(0))
data$log_gdp <- log(data$e_gdppc + 1)
data$log_pop <- log(data$e_pop + 1)

# Create binary Autocracy indicator: 1 if v2x_regime < 2, 0 if v2x_regime >= 2
# (If v2x_regime is NA, autocracy will also be NA)
data$autocracy <- ifelse(!is.na(data$v2x_regime) & data$v2x_regime < 2, 1,
                        ifelse(!is.na(data$v2x_regime) & data$v2x_regime >= 2, 0, NA))

# Show summary of new variables
cat("\nSummary of log_gdp, log_pop, autocracy:\n")
print(summary(data[, c("log_gdp", "log_pop", "autocracy")]))

# Save the operationalized data
output_path <- "Data/cy_operationalized.rds"
saveRDS(data, output_path)
cat("\nOperationalized data saved as:", output_path, "\n")
