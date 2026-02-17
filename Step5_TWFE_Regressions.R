# ============================================================================
# Step5_TWFE_Regressions.R
# Two-way fixed effects regressions: cssmean ~ lagged treatments
# Panel A: v2x_polyarchy as treatment (+ GDP, population controls)
# Panel B: autocracy as treatment (+ GDP, population controls)
# Lags: 2, 3, and 4 years
# Output: LaTeX tables in Tables/ folder
# ============================================================================
rm(list = ls())

if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
library(fixest)

# --- Load operationalized data ---
input_path <- "Data/cy_cssmean_operationalized.rds"
if (!file.exists(input_path)) stop(paste0("Run Step4 first. File not found: ", input_path))
data <- readRDS(input_path)
cat("Loaded:", input_path, "(", nrow(data), "rows )\n\n")

# --- Create Tables directory ---
dir.create("Tables", showWarnings = FALSE)

# --- Set fixest dictionary for nice labels ---
setFixest_dict(c(
  cssmean = "CSS Mean",
  v2x_polyarchy_lag2 = "Polyarchy (t-2)",
  v2x_polyarchy_lag3 = "Polyarchy (t-3)",
  v2x_polyarchy_lag4 = "Polyarchy (t-4)",
  autocracy_lag2 = "Autocracy (t-2)",
  autocracy_lag3 = "Autocracy (t-3)",
  autocracy_lag4 = "Autocracy (t-4)",
  log_gdp_lag2 = "log(GDP p.c.) (t-2)",
  log_gdp_lag3 = "log(GDP p.c.) (t-3)",
  log_gdp_lag4 = "log(GDP p.c.) (t-4)",
  log_pop_lag2 = "log(Population) (t-2)",
  log_pop_lag3 = "log(Population) (t-3)",
  log_pop_lag4 = "log(Population) (t-4)",
  vdem_code = "Country",
  year = "Year"
))

# ============================================================================
# PANEL A: v2x_polyarchy as treatment
# ============================================================================
cat("=== PANEL A: Polyarchy models ===\n\n")

# Lag 2
m_poly_2 <- feols(cssmean ~ v2x_polyarchy_lag2 + log_gdp_lag2 + log_pop_lag2
                   | vdem_code + year, data = data, vcov = "hetero")

# Lag 3
m_poly_3 <- feols(cssmean ~ v2x_polyarchy_lag3 + log_gdp_lag3 + log_pop_lag3
                   | vdem_code + year, data = data, vcov = "hetero")

# Lag 4
m_poly_4 <- feols(cssmean ~ v2x_polyarchy_lag4 + log_gdp_lag4 + log_pop_lag4
                   | vdem_code + year, data = data, vcov = "hetero")

cat("--- Polyarchy lag 2 ---\n")
summary(m_poly_2)
cat("\n--- Polyarchy lag 3 ---\n")
summary(m_poly_3)
cat("\n--- Polyarchy lag 4 ---\n")
summary(m_poly_4)

# LaTeX table
etable(m_poly_2, m_poly_3, m_poly_4,
       title = "TWFE: CSS Mean and Polyarchy (Lagged)",
       headers = c("Lag 2", "Lag 3", "Lag 4"),
       depvar = TRUE,
       fixef.group = list("Country FE" = "vdem_code", "Year FE" = "year"),
       se.below = TRUE,
       fitstat = c("n", "r2", "ar2"),
       file = "Tables/TWFE_Polyarchy.tex",
       replace = TRUE,
       style.tex = style.tex("aer"),
       label = "tab:twfe_polyarchy",
       notes = "Heteroskedasticity-robust standard errors in parentheses. Dependent variable: CSS Mean.")

cat("\nSaved: Tables/TWFE_Polyarchy.tex\n\n")

# ============================================================================
# PANEL B: Autocracy (binary) as treatment
# ============================================================================
cat("=== PANEL B: Autocracy models ===\n\n")

# Lag 2
m_auto_2 <- feols(cssmean ~ autocracy_lag2 + log_gdp_lag2 + log_pop_lag2
                   | vdem_code + year, data = data, vcov = "hetero")

# Lag 3
m_auto_3 <- feols(cssmean ~ autocracy_lag3 + log_gdp_lag3 + log_pop_lag3
                   | vdem_code + year, data = data, vcov = "hetero")

# Lag 4
m_auto_4 <- feols(cssmean ~ autocracy_lag4 + log_gdp_lag4 + log_pop_lag4
                   | vdem_code + year, data = data, vcov = "hetero")

cat("--- Autocracy lag 2 ---\n")
summary(m_auto_2)
cat("\n--- Autocracy lag 3 ---\n")
summary(m_auto_3)
cat("\n--- Autocracy lag 4 ---\n")
summary(m_auto_4)

# LaTeX table
etable(m_auto_2, m_auto_3, m_auto_4,
       title = "TWFE: CSS Mean and Autocracy (Lagged)",
       headers = c("Lag 2", "Lag 3", "Lag 4"),
       depvar = TRUE,
       fixef.group = list("Country FE" = "vdem_code", "Year FE" = "year"),
       se.below = TRUE,
       fitstat = c("n", "r2", "ar2"),
       file = "Tables/TWFE_Autocracy.tex",
       replace = TRUE,
       style.tex = style.tex("aer"),
       label = "tab:twfe_autocracy",
       notes = "Heteroskedasticity-robust standard errors in parentheses. Dependent variable: CSS Mean. Autocracy = 1 if v2x\\_regime $<$ 2.")

cat("\nSaved: Tables/TWFE_Autocracy.tex\n\n")

cat("=== All regressions complete ===\n")
