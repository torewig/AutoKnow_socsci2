# ============================================================================
# Step7_SynthControl.R
# Identify suitable cases for synthetic control analysis of cssmean with
# transition to autocracy as treatment. Run synthetic control for each
# selected case and produce figures.
# ============================================================================
rm(list = ls())

if (!requireNamespace("Synth", quietly = TRUE)) install.packages("Synth")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(Synth)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Load operationalized data ---
input_path <- "Data/cy_cssmean_operationalized.rds"
if (!file.exists(input_path)) stop(paste0("Run Step4 first. File not found: ", input_path))
data <- readRDS(input_path)
cat("Loaded:", input_path, "(", nrow(data), "rows )\n\n")

dir.create("Figures", showWarnings = FALSE)

# ============================================================================
# PART 1: Identify candidate transitions
# ============================================================================
cat("============================================================\n")
cat("PART 1: IDENTIFYING AUTOCRACY TRANSITION CANDIDATES\n")
cat("============================================================\n\n")

data <- data %>% arrange(country, year)

# Detect transitions: autocracy goes from 0 to 1
data <- data %>%
  group_by(country) %>%
  mutate(auto_lag = lag(autocracy)) %>%
  ungroup()

transitions <- data %>%
  filter(!is.na(auto_lag) & auto_lag == 0 & autocracy == 1) %>%
  select(country, year)

# Assess each transition: count pre/post years with non-missing cssmean
candidates <- data.frame()
for (i in seq_len(nrow(transitions))) {
  ctry <- transitions$country[i]
  tyear <- transitions$year[i]
  ctry_data <- data %>% filter(country == ctry & !is.na(cssmean))
  n_pre <- sum(ctry_data$year < tyear)
  n_post <- sum(ctry_data$year >= tyear)
  candidates <- rbind(candidates, data.frame(
    country = ctry, transition_year = tyear,
    n_pre = n_pre, n_post = n_post,
    year_min = min(ctry_data$year), year_max = max(ctry_data$year),
    stringsAsFactors = FALSE
  ))
}

# Filter: at least 4 years pre and 3 years post
good_cases <- candidates %>% filter(n_pre >= 4 & n_post >= 3)

cat("All transitions to autocracy:", nrow(candidates), "\n")
cat("Suitable cases (>= 4 pre, >= 3 post):", nrow(good_cases), "\n\n")
print(as.data.frame(good_cases), row.names = FALSE)

# For countries with multiple transitions, keep the first one with sufficient data
good_cases <- good_cases %>%
  group_by(country) %>%
  slice_min(transition_year, n = 1) %>%
  ungroup()

cat("\nAfter keeping first transition per country:", nrow(good_cases), "cases\n")
print(as.data.frame(good_cases), row.names = FALSE)

# ============================================================================
# PART 2: Run synthetic control for each selected case
# ============================================================================
cat("\n============================================================\n")
cat("PART 2: RUNNING SYNTHETIC CONTROL ANALYSES\n")
cat("============================================================\n\n")

# Donor pool: countries that remain democratic (autocracy == 0) throughout
always_dem <- data %>%
  filter(!is.na(autocracy)) %>%
  group_by(country) %>%
  summarise(ever_autocracy = max(autocracy), .groups = "drop") %>%
  filter(ever_autocracy == 0) %>%
  pull(country)

cat("Donor pool (always-democratic countries):", length(always_dem), "\n\n")

# Function to run synthetic control for one case
run_synth <- function(treat_country, treat_year, data, donor_countries,
                      min_pre_window = 5) {

  cat(sprintf("--- Running synth for %s (treatment year: %d) ---\n",
              treat_country, treat_year))

  # Define analysis window: use available data, but at least min_pre_window before
  treat_data <- data %>% filter(country == treat_country & !is.na(cssmean))
  earliest <- max(min(treat_data$year), treat_year - 15)  # cap at 15 years pre
  latest <- max(treat_data$year)

  pre_period <- earliest:(treat_year - 1)
  post_period <- treat_year:latest
  all_years <- earliest:latest

  # Build panel: treated + donors
  panel <- data %>%
    filter(country %in% c(treat_country, donor_countries)) %>%
    filter(year %in% all_years)

  # Balance the panel
  complete_panel <- expand.grid(
    country = unique(panel$country), year = all_years, stringsAsFactors = FALSE)
  panel <- merge(complete_panel, panel, by = c("country", "year"), all.x = TRUE)

  # Keep only donors with complete pre-period cssmean
  pre_coverage <- panel %>%
    filter(year %in% pre_period) %>%
    group_by(country) %>%
    summarise(n_nonmiss = sum(!is.na(cssmean)), .groups = "drop")

  n_pre <- length(pre_period)
  complete_donors <- pre_coverage %>%
    filter(n_nonmiss == n_pre & country != treat_country) %>%
    pull(country)

  # Also require treated unit to have complete pre-period
  treat_pre <- pre_coverage %>% filter(country == treat_country)
  if (nrow(treat_pre) == 0 || treat_pre$n_nonmiss < n_pre) {
    cat("  SKIP: treated unit has incomplete pre-period data\n\n")
    return(NULL)
  }

  if (length(complete_donors) < 3) {
    cat(sprintf("  SKIP: only %d donors with complete pre-period\n\n",
                length(complete_donors)))
    return(NULL)
  }

  panel <- panel %>% filter(country %in% c(treat_country, complete_donors))
  panel$unit_id <- as.numeric(as.factor(panel$country))

  treat_id <- unique(panel$unit_id[panel$country == treat_country])
  control_ids <- unique(panel$unit_id[panel$country != treat_country])

  cat(sprintf("  Pre-period: %d-%d (%d yrs), Post-period: %d-%d (%d yrs)\n",
              min(pre_period), max(pre_period), length(pre_period),
              min(post_period), max(post_period), length(post_period)))
  cat(sprintf("  Donors with complete pre-period: %d\n", length(complete_donors)))

  # Run Synth
  tryCatch({
    dp <- dataprep(
      foo = as.data.frame(panel),
      predictors = "cssmean",
      predictors.op = "mean",
      dependent = "cssmean",
      unit.variable = "unit_id",
      time.variable = "year",
      treatment.identifier = treat_id,
      controls.identifier = control_ids,
      time.predictors.prior = pre_period,
      time.optimize.ssr = pre_period,
      unit.names.variable = "country",
      time.plot = all_years
    )

    so <- synth(dp, verbose = FALSE)

    # Extract results
    actual <- as.numeric(dp$Y1plot)
    synthetic <- as.numeric(dp$Y0plot %*% so$solution.w)
    gap <- actual - synthetic

    # Pre-treatment RMSPE
    pre_idx <- which(all_years %in% pre_period)
    rmspe <- sqrt(mean(gap[pre_idx]^2))

    # Donor weights
    weights <- data.frame(
      country = complete_donors,
      weight = as.numeric(so$solution.w)
    ) %>% filter(weight > 0.01) %>% arrange(desc(weight))

    cat(sprintf("  Pre-treatment RMSPE: %.3f\n", rmspe))
    cat("  Top donor weights:\n")
    for (j in seq_len(min(5, nrow(weights)))) {
      cat(sprintf("    %-20s %.3f\n", weights$country[j], weights$weight[j]))
    }
    cat("\n")

    list(
      country = treat_country,
      treat_year = treat_year,
      years = all_years,
      actual = actual,
      synthetic = synthetic,
      gap = gap,
      rmspe = rmspe,
      weights = weights
    )
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n\n", e$message))
    return(NULL)
  })
}

# Run for all good cases
results <- list()
for (i in seq_len(nrow(good_cases))) {
  res <- run_synth(
    treat_country = good_cases$country[i],
    treat_year = good_cases$transition_year[i],
    data = data,
    donor_countries = always_dem
  )
  if (!is.null(res)) {
    results[[length(results) + 1]] <- res
  }
}

cat(sprintf("\nSuccessful synthetic control analyses: %d / %d\n\n",
            length(results), nrow(good_cases)))

# ============================================================================
# PART 3: Figures
# ============================================================================
cat("============================================================\n")
cat("PART 3: GENERATING FIGURES\n")
cat("============================================================\n\n")

# --- Figure 8: Individual country plots (actual vs synthetic) ---
for (res in results) {
  plot_df <- data.frame(
    year = res$years,
    Actual = res$actual,
    Synthetic = res$synthetic
  ) %>%
    pivot_longer(cols = c("Actual", "Synthetic"),
                 names_to = "Series", values_to = "CSSmean")

  p <- ggplot(plot_df, aes(x = year, y = CSSmean, color = Series, linetype = Series)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = res$treat_year, linetype = "dashed", color = "red",
               linewidth = 0.8) +
    scale_color_manual(values = c("Actual" = "#2166AC", "Synthetic" = "#D6604D")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Synthetic" = "dashed")) +
    annotate("text", x = res$treat_year, y = Inf, label = paste("Treatment:", res$treat_year),
             hjust = -0.1, vjust = 1.5, color = "red", size = 3.5) +
    labs(x = "Year", y = "CSS Mean",
         title = paste0("Synthetic Control: ", res$country),
         subtitle = sprintf("Transition to autocracy in %d (pre-RMSPE: %.2f)",
                            res$treat_year, res$rmspe),
         color = NULL, linetype = NULL) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")

  fname <- sprintf("Figures/fig_synth_%s.png",
                    gsub(" ", "_", tolower(res$country)))
  ggsave(fname, p, width = 8, height = 5, dpi = 300)
  cat("  Saved:", fname, "\n")
}

# --- Figure 9: Gap plots (actual - synthetic) for all cases ---
gap_list <- lapply(results, function(res) {
  data.frame(
    country = res$country,
    year = res$years,
    gap = res$gap,
    treat_year = res$treat_year,
    rel_year = res$years - res$treat_year
  )
})
gap_df <- bind_rows(gap_list)

p_gaps <- ggplot(gap_df, aes(x = rel_year, y = gap, color = country)) +
  geom_line(linewidth = 0.7, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
  labs(x = "Years from transition to autocracy",
       y = "Gap (Actual - Synthetic CSS Mean)",
       title = "Synthetic Control Gaps: All Cases",
       subtitle = "Positive = actual CSSmean above synthetic counterfactual",
       color = "Country") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave("Figures/fig_synth_gaps_all.png", p_gaps, width = 10, height = 6, dpi = 300)
cat("  Saved: Figures/fig_synth_gaps_all.png\n")

# --- Figure 10: Average gap across all cases ---
avg_gap <- gap_df %>%
  group_by(rel_year) %>%
  summarise(mean_gap = mean(gap, na.rm = TRUE),
            se_gap = sd(gap, na.rm = TRUE) / sqrt(n()),
            n = n(), .groups = "drop") %>%
  filter(n >= 3)  # only plot where at least 3 cases overlap

p_avg_gap <- ggplot(avg_gap, aes(x = rel_year, y = mean_gap)) +
  geom_ribbon(aes(ymin = mean_gap - 1.96 * se_gap,
                  ymax = mean_gap + 1.96 * se_gap),
              alpha = 0.2, fill = "#2166AC") +
  geom_line(linewidth = 1, color = "#2166AC") +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
  labs(x = "Years from transition to autocracy",
       y = "Mean gap (Actual - Synthetic CSS Mean)",
       title = "Average Synthetic Control Gap Across All Cases",
       subtitle = "Shaded area: 95% CI. Positive = CSSmean above counterfactual.") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figures/fig_synth_avg_gap.png", p_avg_gap, width = 8, height = 5, dpi = 300)
cat("  Saved: Figures/fig_synth_avg_gap.png\n")

# --- Figure 11: Faceted panel of all individual cases ---
# Prepare data for faceting
facet_list <- lapply(results, function(res) {
  data.frame(
    country = res$country,
    year = res$years,
    Actual = res$actual,
    Synthetic = res$synthetic,
    treat_year = res$treat_year
  )
})
facet_df <- bind_rows(facet_list) %>%
  pivot_longer(cols = c("Actual", "Synthetic"),
               names_to = "Series", values_to = "CSSmean")

# Vertical line data for facets
vline_df <- distinct(facet_df, country, treat_year)

p_facet <- ggplot(facet_df, aes(x = year, y = CSSmean, color = Series, linetype = Series)) +
  geom_line(linewidth = 0.7) +
  geom_vline(data = vline_df, aes(xintercept = treat_year),
             linetype = "dashed", color = "red", linewidth = 0.5) +
  scale_color_manual(values = c("Actual" = "#2166AC", "Synthetic" = "#D6604D")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Synthetic" = "dashed")) +
  facet_wrap(~ country, scales = "free_x", ncol = 4) +
  labs(x = "Year", y = "CSS Mean",
       title = "Synthetic Control: All Autocracy Transitions",
       subtitle = "Red dashed line = transition year",
       color = NULL, linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 10))

# Adjust height based on number of cases
n_cases <- length(results)
n_rows <- ceiling(n_cases / 4)
fig_height <- max(5, n_rows * 3.2)

ggsave("Figures/fig_synth_facet_all.png", p_facet,
       width = 14, height = fig_height, dpi = 300)
cat("  Saved: Figures/fig_synth_facet_all.png\n")

# ============================================================================
# Summary table to console
# ============================================================================
cat("\n============================================================\n")
cat("SUMMARY OF SYNTHETIC CONTROL RESULTS\n")
cat("============================================================\n")
cat(sprintf("%-20s %6s %8s %10s %12s\n",
            "Country", "Treat", "Pre-yrs", "RMSPE", "Post-gap(avg)"))
cat(paste(rep("-", 60), collapse = ""), "\n")
for (res in results) {
  post_idx <- which(res$years >= res$treat_year)
  avg_post_gap <- mean(res$gap[post_idx])
  n_pre <- sum(res$years < res$treat_year)
  cat(sprintf("%-20s %6d %8d %10.3f %12.3f\n",
              res$country, res$treat_year, n_pre, res$rmspe, avg_post_gap))
}

cat("\n=== Step 7 complete ===\n")
