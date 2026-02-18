# ============================================================================
# Step8_PanelMatch.R
# Panel matching analysis: ATT of autocracy on CSSmean
# Uses PanelMatch v3+ API with Mahalanobis refinement on GDP and population
# ============================================================================
rm(list = ls())

if (!requireNamespace("PanelMatch", quietly = TRUE)) install.packages("PanelMatch")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(PanelMatch)
library(dplyr)
library(ggplot2)

# --- Load operationalized data ---
input_path <- "Data/cy_cssmean_operationalized.rds"
if (!file.exists(input_path)) stop(paste0("Run Step4 first. File not found: ", input_path))
data <- readRDS(input_path)
cat("Loaded:", input_path, "(", nrow(data), "rows )\n\n")

dir.create("Figures", showWarnings = FALSE)
dir.create("Tables", showWarnings = FALSE)

# ============================================================================
# PART 1: Prepare panel data
# ============================================================================
cat("============================================================\n")
cat("PART 1: PREPARING PANEL DATA\n")
cat("============================================================\n\n")

pm_data <- data %>%
  select(country, vdem_code, year, autocracy, cssmean, log_gdp, log_pop) %>%
  filter(!is.na(autocracy) & !is.na(cssmean))

pm_data$year <- as.integer(pm_data$year)

# Keep only countries with consecutive years (no gaps)
country_info <- pm_data %>%
  group_by(country) %>%
  summarise(
    min_year = min(year), max_year = max(year),
    n_years = n(),
    expected_years = max(year) - min(year) + 1,
    .groups = "drop"
  )

consecutive <- country_info %>% filter(n_years == expected_years)
pm_data <- pm_data %>% filter(country %in% consecutive$country)

cat("Countries with consecutive years:", nrow(consecutive), "\n")
cat("Observations after filtering:", nrow(pm_data), "\n")
cat("Year range:", min(pm_data$year), "-", max(pm_data$year), "\n\n")

pm_data$unit_id <- as.numeric(as.factor(pm_data$country))
pm_data$log_gdp <- as.numeric(pm_data$log_gdp)
pm_data$log_pop <- as.numeric(pm_data$log_pop)
pm_data$autocracy <- as.integer(pm_data$autocracy)

# Create PanelData object (PanelMatch v3 API)
pd <- PanelData(
  panel.data = as.data.frame(pm_data),
  unit.id = "unit_id",
  time.id = "year",
  treatment = "autocracy",
  outcome = "cssmean"
)

cat("PanelData object created.\n")
cat("  Units:", length(unique(pm_data$unit_id)), "\n")
cat("  Autocracy obs:", sum(pm_data$autocracy == 1), "\n")
cat("  Democracy obs:", sum(pm_data$autocracy == 0), "\n\n")

# ============================================================================
# PART 2: PanelMatch — ATT with Mahalanobis matching
# ============================================================================
cat("============================================================\n")
cat("PART 2: PANEL MATCHING — ATT OF AUTOCRACY ON CSSMEAN\n")
cat("============================================================\n\n")

# Helper: extract estimates from summary matrix (PanelMatch v3)
extract_estimates <- function(est_obj, leads) {
  s <- summary(est_obj)  # returns a matrix with columns: estimate, std.error, 2.5%, 97.5%
  data.frame(
    lead = leads,
    att = s[, "estimate"],
    se = s[, "std.error"],
    ci_lower = s[, "2.5%"],
    ci_upper = s[, "97.5%"]
  )
}

# Helper: make LaTeX rows from summary matrix
make_tex_rows <- function(est_obj, leads, label) {
  s <- summary(est_obj)
  rows <- character()
  for (i in seq_along(leads)) {
    est_val <- s[i, "estimate"]
    ci_lo <- s[i, "2.5%"]
    ci_hi <- s[i, "97.5%"]
    sig <- ifelse(ci_lo > 0 | ci_hi < 0, "*", "")
    rows <- c(rows, sprintf("  %s & Lead %d & %.3f%s & [%.3f, %.3f] \\\\",
                            ifelse(i == 1, label, ""), leads[i],
                            est_val, sig, ci_lo, ci_hi))
  }
  rows
}

# --- Model A: lag = 1, leads 0-5 ---
cat("--- Model A: lag = 1, leads 0:5 ---\n")
pm_a <- PanelMatch(
  panel.data = pd, lag = 1,
  refinement.method = "mahalanobis", qoi = "att",
  size.match = 5, match.missing = TRUE,
  covs.formula = ~ log_gdp + log_pop,
  lead = 0:5, forbid.treatment.reversal = FALSE, verbose = FALSE
)

est_a <- PanelEstimate(
  sets = pm_a, panel.data = pd,
  number.iterations = 1000, confidence.level = 0.95, se.method = "bootstrap"
)

cat("ATT estimates (lag=1):\n")
print(summary(est_a))
cat("\n")

# --- Model B: lag = 2, leads 0:5 (with placebo) ---
cat("--- Model B: lag = 2, leads 0:5 ---\n")
pm_b <- PanelMatch(
  panel.data = pd, lag = 2,
  refinement.method = "mahalanobis", qoi = "att",
  size.match = 5, match.missing = TRUE,
  covs.formula = ~ log_gdp + log_pop,
  lead = 0:5, forbid.treatment.reversal = FALSE, verbose = FALSE,
  placebo.test = TRUE
)

est_b <- PanelEstimate(
  sets = pm_b, panel.data = pd,
  number.iterations = 1000, confidence.level = 0.95, se.method = "bootstrap"
)

cat("ATT estimates (lag=2):\n")
print(summary(est_b))
cat("\n")

# --- Model C: lag = 3, leads 0:5 ---
cat("--- Model C: lag = 3, leads 0:5 ---\n")
pm_c <- PanelMatch(
  panel.data = pd, lag = 3,
  refinement.method = "mahalanobis", qoi = "att",
  size.match = 5, match.missing = TRUE,
  covs.formula = ~ log_gdp + log_pop,
  lead = 0:5, forbid.treatment.reversal = FALSE, verbose = FALSE
)

est_c <- PanelEstimate(
  sets = pm_c, panel.data = pd,
  number.iterations = 1000, confidence.level = 0.95, se.method = "bootstrap"
)

cat("ATT estimates (lag=3):\n")
print(summary(est_c))
cat("\n")

# ============================================================================
# PART 3: Placebo test
# ============================================================================
cat("============================================================\n")
cat("PART 3: PLACEBO TEST\n")
cat("============================================================\n\n")

placebo_b <- placebo_test(
  pm.obj = pm_b, panel.data = pd,
  number.iterations = 1000, confidence.level = 0.95, se.method = "bootstrap"
)

cat("Placebo test (lag=2):\n")
cat("  Estimate:", placebo_b$estimates, "\n")
cat("  SE:", placebo_b$standard.errors, "\n")
ci_lo_plac <- placebo_b$estimates - 1.96 * placebo_b$standard.errors
ci_hi_plac <- placebo_b$estimates + 1.96 * placebo_b$standard.errors
cat("  95% CI: [", round(ci_lo_plac, 3), ",", round(ci_hi_plac, 3), "]\n")
cat("  Includes zero:", ifelse(ci_lo_plac <= 0 & ci_hi_plac >= 0, "YES (parallel trends supported)", "NO"), "\n\n")

# ============================================================================
# PART 4: Figures
# ============================================================================
cat("============================================================\n")
cat("PART 4: GENERATING FIGURES\n")
cat("============================================================\n\n")

df_a <- extract_estimates(est_a, 0:5); df_a$model <- "Lag 1"
df_b <- extract_estimates(est_b, 0:5); df_b$model <- "Lag 2"
df_c <- extract_estimates(est_c, 0:5); df_c$model <- "Lag 3"
df_all <- rbind(df_a, df_b, df_c)

# --- Figure: ATT for all three lag specifications ---
p_att <- ggplot(df_all, aes(x = lead, y = att, color = model)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, alpha = 0.6) +
  scale_color_manual(values = c("Lag 1" = "#D6604D", "Lag 2" = "#2166AC", "Lag 3" = "#4DAF4A")) +
  labs(x = "Lead (years after treatment)", y = "ATT (effect on CSS Mean)",
       title = "PanelMatch: ATT of Autocracy on Social Science Output",
       subtitle = "Mahalanobis matching on log GDP and log population, 95% bootstrap CI",
       color = "Matching\nlag") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("Figures/fig_panelmatch_att.png", p_att, width = 8, height = 5.5, dpi = 300)
cat("  Saved: Figures/fig_panelmatch_att.png\n")

# --- Figure: Preferred specification (lag=2) with CI ribbon ---
p_lag2 <- ggplot(df_b, aes(x = lead, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#2166AC") +
  geom_line(linewidth = 1, color = "#2166AC") +
  geom_point(size = 3, color = "#2166AC") +
  labs(x = "Lead (years after treatment)", y = "ATT (effect on CSS Mean)",
       title = "PanelMatch: ATT of Autocracy on CSS Mean (Lag = 2)",
       subtitle = "Mahalanobis matching on log GDP and log population, 95% bootstrap CI") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figures/fig_panelmatch_att_lag2.png", p_lag2, width = 8, height = 5, dpi = 300)
cat("  Saved: Figures/fig_panelmatch_att_lag2.png\n")

# --- Figure: Combined ATT (lag=2) with placebo point ---
# The placebo test in v3 returns a single pre-treatment estimate
df_prepost <- rbind(
  data.frame(x = -1, y = placebo_b$estimates,
             ci_lo = ci_lo_plac, ci_hi = ci_hi_plac,
             period = "Pre-treatment (placebo)"),
  data.frame(x = df_b$lead, y = df_b$att,
             ci_lo = df_b$ci_lower, ci_hi = df_b$ci_upper,
             period = "Post-treatment (ATT)")
)

p_prepost <- ggplot(df_prepost, aes(x = x, y = y, color = period, fill = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.3, alpha = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Pre-treatment (placebo)" = "#D6604D",
                                "Post-treatment (ATT)" = "#2166AC")) +
  scale_fill_manual(values = c("Pre-treatment (placebo)" = "#D6604D",
                               "Post-treatment (ATT)" = "#2166AC")) +
  labs(x = "Years relative to treatment",
       y = "Effect on CSS Mean",
       title = "PanelMatch: Pre- and Post-Treatment Effects of Autocracy",
       subtitle = "Lag = 2, Mahalanobis matching on log GDP and log population",
       color = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("Figures/fig_panelmatch_prepost.png", p_prepost, width = 9, height = 5.5, dpi = 300)
cat("  Saved: Figures/fig_panelmatch_prepost.png\n")

# ============================================================================
# PART 5: LaTeX table
# ============================================================================
cat("\n--- Generating LaTeX table ---\n")

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\caption{\\label{tab:panelmatch} PanelMatch: ATT of Autocracy on CSS Mean}",
  "\\bigskip",
  "\\centering",
  "\\begin{tabular}{llrl}",
  "\\toprule",
  "Specification & Lead & ATT & 95\\% Bootstrap CI \\\\",
  "\\midrule",
  make_tex_rows(est_a, 0:5, "Lag 1"),
  "\\midrule",
  make_tex_rows(est_b, 0:5, "Lag 2"),
  "\\midrule",
  make_tex_rows(est_c, 0:5, "Lag 3"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par \\raggedright",
  "Mahalanobis matching on log GDP per capita and log population. * indicates 95\\% CI excludes zero. 1,000 bootstrap iterations.",
  "\\end{table}"
)
writeLines(tex_lines, "Tables/PanelMatch_ATT.tex")
cat("  Saved: Tables/PanelMatch_ATT.tex\n")

cat("\n=== Step 8 complete ===\n")
