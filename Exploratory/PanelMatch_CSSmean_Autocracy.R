# ============================================================================
# PanelMatch_CSSmean_Autocracy.R
# Panel matching analysis: cssmean ~ autocracy
# ============================================================================

# Install and load PanelMatch if needed
if (!requireNamespace("PanelMatch", quietly = TRUE)) install.packages("PanelMatch")
library(PanelMatch)
library(dplyr)

# Load operationalized data
input_path <- "../Data/cy_operationalized.rds"
if (!file.exists(input_path)) stop(paste("File not found:", input_path))
data <- readRDS(input_path)

# Prepare data: must have unit id, time, treatment, and outcome
# We'll use country as unit, year as time, autocracy as treatment, cssmean as outcome
# Remove rows with missing values in key variables
pm_data <- data %>%
  select(country, year, autocracy, cssmean, log_gdp, log_pop) %>%
  filter(!is.na(country) & !is.na(year) & !is.na(autocracy) & !is.na(cssmean))

# Ensure year is integer
pm_data$year <- as.integer(pm_data$year)

# Identify countries with consecutive years (no gaps)
country_years <- pm_data %>% group_by(country) %>% summarise(
  min_year = min(year), max_year = max(year), n_years = n(),
  expected_years = max(year) - min(year) + 1
)
consecutive_countries <- country_years %>% filter(n_years == expected_years) %>% pull(country)
pm_data <- pm_data %>% filter(country %in% consecutive_countries)

# Ensure all covariates are numeric
pm_data$log_gdp <- as.numeric(pm_data$log_gdp)
pm_data$log_pop <- as.numeric(pm_data$log_pop)

# PanelMatch requires numeric unit id
pm_data <- pm_data %>% mutate(unit_id = as.numeric(as.factor(country)))

# Ensure all required columns exist and are numeric
required_cols <- c("unit_id", "year", "autocracy", "cssmean", "log_gdp", "log_pop")
missing_cols <- setdiff(required_cols, names(pm_data))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}
for (col in required_cols) {
  if (!is.numeric(pm_data[[col]])) {
    pm_data[[col]] <- as.numeric(pm_data[[col]])
  }
}

# Confirm columns before PanelMatch
cat("[DEBUG] Columns in pm_data before PanelMatch:\n")
print(names(pm_data))

# Run panel match (5-year lead, 1-year lag, match on log_gdp and log_pop)
pm_result <- PanelMatch(
  lag = 1,
  time.id = "year",
  unit.id = "unit_id",
  treatment = "autocracy",
  refinement.method = "mahalanobis",
  data = pm_data,
  match.missing = TRUE,
  covs.formula = ~ log_gdp + log_pop,
  lead = 1:5,  # estimate ATT for 1 to 5 years after treatment
  outcome.var = "cssmean",
  qoi = "att",
  forbid.treatment.reversal = TRUE,
  size.match = 5
)

# Estimate effects for all leads
# Confirm columns before PanelEstimate
cat("[DEBUG] Columns in pm_data before PanelEstimate:\n")
print(names(pm_data))
pm_estimate <- PanelEstimate(
  sets = pm_result,
  data = pm_data
)

# Print summary and plot for all leads
print(summary(pm_estimate))
plot(pm_estimate, main = "PanelMatch: ATT of Autocracy on CSSmean (Leads 1-5)")
ggsave("PanelMatch_ATT_cssmean_leads1-5.png")
cat("PanelMatch plot saved as PanelMatch_ATT_cssmean_leads1-5.png\n")

# Estimate post-treatment effects (leads 1:5) with lag = 2
cat("[DEBUG] Columns in pm_data before PanelMatch (post):\n")
print(names(pm_data))
pm_result_post <- PanelMatch(
  lag = 2,
  time.id = "year",
  unit.id = "unit_id",
  treatment = "autocracy",
  refinement.method = "mahalanobis",
  data = pm_data,
  match.missing = TRUE,
  covs.formula = ~ log_gdp + log_pop,
  lead = 1:5,
  outcome.var = "cssmean",
  qoi = "att",
  forbid.treatment.reversal = TRUE,
  size.match = 5
)
cat("[DEBUG] Columns in pm_data before PanelEstimate (post):\n")
print(names(pm_data))
pm_estimate_post <- PanelEstimate(sets = pm_result_post, data = pm_data)

# Use placebo_test for pre-treatment (placebo) effects with lag = 2
cat("[DEBUG] Columns in pm_data before placebo_test:\n")
print(names(pm_data))
cat("[DEBUG] pm_result_post unit.id:", pm_result_post$unit.id, "\n")
cat("[DEBUG] pm_result_post time.id:", pm_result_post$time.id, "\n")
cat("[DEBUG] pm_result_post treatment:", pm_result_post$treatment, "\n")
cat("[DEBUG] pm_result_post outcome.var:", pm_result_post$outcome.var, "\n")
placebo <- placebo_test(
  pm.obj = pm_result_post,
  data = pm_data,
  lead = 1:5, # placebo leads correspond to pre-treatment periods
  type = "lag"
)

# Extract placebo (pre-treatment) and post-treatment effects for plotting
att_post <- data.frame(
  period = 1:5,
  att = pm_estimate_post$estimates,
  se = pm_estimate_post$standard.errors
)
att_pre <- data.frame(
  period = -5:-1,
  att = placebo$estimates,
  se = placebo$standard.errors
)
att_all <- rbind(att_pre, att_post)

library(ggplot2)
p <- ggplot(att_all, aes(x = period, y = att)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "PanelMatch: ATT of Autocracy on CSSmean",
       x = "Years from Treatment (0 = treatment year)",
       y = "ATT (cssmean)") +
  theme_minimal(base_size = 14)

print(p)
ggsave("PanelMatch_ATT_cssmean_prepost.png", plot = p, width = 8, height = 5, dpi = 300)
cat("PanelMatch pre/post plot saved as PanelMatch_ATT_cssmean_prepost.png\n")
