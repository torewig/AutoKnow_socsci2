# ============================================================================
# Step6_Descriptives.R
# Descriptive figures and tables exploring the relationships between
# v2x_polyarchy / v2x_regime and cssmean
# Output: Figures/ (PNG) and Tables/ (LaTeX)
# ============================================================================
rm(list = ls())

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# --- Load operationalized data (has autocracy indicator) ---
input_path <- "Data/cy_cssmean_operationalized.rds"
if (!file.exists(input_path)) stop(paste0("Run Step4 first. File not found: ", input_path))
data <- readRDS(input_path)
cat("Loaded:", input_path, "(", nrow(data), "rows )\n\n")

# Create output directories
dir.create("Figures", showWarnings = FALSE)
dir.create("Tables", showWarnings = FALSE)

# Regime type factor with readable labels
data$regime_label <- factor(data$v2x_regime,
                            levels = 0:3,
                            labels = c("Closed\nautocracy",
                                       "Electoral\nautocracy",
                                       "Electoral\ndemocracy",
                                       "Liberal\ndemocracy"))

# ============================================================================
# FIGURE 1: Scatter — v2x_polyarchy vs cssmean
# ============================================================================
cat("--- Figure 1: Polyarchy vs CSSmean scatter ---\n")

p1 <- ggplot(data %>% filter(!is.na(v2x_polyarchy)),
             aes(x = v2x_polyarchy, y = cssmean)) +
  geom_point(alpha = 0.15, size = 1, color = "grey40") +
  geom_smooth(method = "loess", se = TRUE, color = "#2166AC", fill = "#92C5DE") +
  labs(x = "V-DEM Polyarchy Index",
       y = "CSS Mean",
       title = "Polyarchy and Social Science Output",
       subtitle = "Country-year observations with LOESS smoother") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figures/fig1_polyarchy_cssmean_scatter.png", p1, width = 7, height = 5, dpi = 300)
cat("  Saved: Figures/fig1_polyarchy_cssmean_scatter.png\n")

# ============================================================================
# FIGURE 2: Box plots — cssmean by regime type
# ============================================================================
cat("--- Figure 2: CSSmean by regime type ---\n")

p2 <- ggplot(data %>% filter(!is.na(regime_label)),
             aes(x = regime_label, y = cssmean, fill = regime_label)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  scale_fill_manual(values = c("#D6604D", "#F4A582", "#92C5DE", "#2166AC")) +
  labs(x = NULL, y = "CSS Mean",
       title = "Social Science Output by Regime Type",
       subtitle = "V-DEM regime classification (v2x_regime)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("Figures/fig2_cssmean_by_regime_boxplot.png", p2, width = 7, height = 5, dpi = 300)
cat("  Saved: Figures/fig2_cssmean_by_regime_boxplot.png\n")

# ============================================================================
# FIGURE 3: Violin + jitter — cssmean by regime type
# ============================================================================
cat("--- Figure 3: CSSmean by regime type (violin) ---\n")

p3 <- ggplot(data %>% filter(!is.na(regime_label)),
             aes(x = regime_label, y = cssmean, fill = regime_label)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_manual(values = c("#D6604D", "#F4A582", "#92C5DE", "#2166AC")) +
  labs(x = NULL, y = "CSS Mean",
       title = "Distribution of Social Science Output by Regime Type",
       subtitle = "Violin plots with quartile lines") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("Figures/fig3_cssmean_by_regime_violin.png", p3, width = 7, height = 5, dpi = 300)
cat("  Saved: Figures/fig3_cssmean_by_regime_violin.png\n")

# ============================================================================
# FIGURE 4: Binned scatter — polyarchy quintiles vs mean cssmean
# ============================================================================
cat("--- Figure 4: Binned means by polyarchy quintile ---\n")

binned <- data %>%
  filter(!is.na(v2x_polyarchy) & !is.na(cssmean)) %>%
  mutate(poly_bin = cut(v2x_polyarchy,
                        breaks = quantile(v2x_polyarchy, probs = seq(0, 1, 0.1), na.rm = TRUE),
                        include.lowest = TRUE, dig.lab = 2)) %>%
  group_by(poly_bin) %>%
  summarise(mean_css = mean(cssmean),
            se_css = sd(cssmean) / sqrt(n()),
            mean_poly = mean(v2x_polyarchy),
            n = n(), .groups = "drop")

p4 <- ggplot(binned, aes(x = mean_poly, y = mean_css)) +
  geom_pointrange(aes(ymin = mean_css - 1.96 * se_css,
                      ymax = mean_css + 1.96 * se_css),
                  size = 0.6, color = "#2166AC") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey50") +
  labs(x = "Mean Polyarchy (decile bin)",
       y = "Mean CSS Mean (+/- 95% CI)",
       title = "Social Science Output by Polyarchy Decile",
       subtitle = "Binned scatter with 95% confidence intervals") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("Figures/fig4_binned_polyarchy_cssmean.png", p4, width = 7, height = 5, dpi = 300)
cat("  Saved: Figures/fig4_binned_polyarchy_cssmean.png\n")

# ============================================================================
# FIGURE 5: Time trends — mean cssmean by regime type over time
# ============================================================================
cat("--- Figure 5: CSSmean trends over time by regime type ---\n")

year_regime <- data %>%
  filter(!is.na(regime_label) & !is.na(cssmean) & year >= 1990) %>%
  group_by(year, regime_label) %>%
  summarise(mean_css = mean(cssmean, na.rm = TRUE),
            n = n(), .groups = "drop")

p5 <- ggplot(year_regime, aes(x = year, y = mean_css, color = regime_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(aes(size = n), alpha = 0.6) +
  scale_color_manual(values = c("#D6604D", "#F4A582", "#92C5DE", "#2166AC")) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  labs(x = "Year", y = "Mean CSS Mean", color = "Regime type",
       title = "Social Science Output Over Time by Regime Type",
       subtitle = "1990-2019, point size proportional to N") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("Figures/fig5_cssmean_trends_by_regime.png", p5, width = 9, height = 5.5, dpi = 300)
cat("  Saved: Figures/fig5_cssmean_trends_by_regime.png\n")

# ============================================================================
# FIGURE 6: Scatter — polyarchy vs cssmean, colored by regime type
# ============================================================================
cat("--- Figure 6: Polyarchy vs CSSmean colored by regime ---\n")

p6 <- ggplot(data %>% filter(!is.na(v2x_polyarchy) & !is.na(regime_label)),
             aes(x = v2x_polyarchy, y = cssmean, color = regime_label)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#D6604D", "#F4A582", "#92C5DE", "#2166AC")) +
  labs(x = "V-DEM Polyarchy Index", y = "CSS Mean", color = "Regime type",
       title = "Polyarchy and Social Science Output by Regime Type",
       subtitle = "Separate linear fits per regime category") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("Figures/fig6_polyarchy_cssmean_by_regime.png", p6, width = 8, height = 5.5, dpi = 300)
cat("  Saved: Figures/fig6_polyarchy_cssmean_by_regime.png\n")

# ============================================================================
# FIGURE 7: Heatmap — mean cssmean by polyarchy quartile x decade
# ============================================================================
cat("--- Figure 7: Heatmap polyarchy quartile x decade ---\n")

heatmap_data <- data %>%
  filter(!is.na(v2x_polyarchy) & !is.na(cssmean) & year >= 1990) %>%
  mutate(decade = paste0(floor(year / 10) * 10, "s"),
         poly_quartile = cut(v2x_polyarchy,
                             breaks = quantile(v2x_polyarchy, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                             include.lowest = TRUE,
                             labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))) %>%
  group_by(decade, poly_quartile) %>%
  summarise(mean_css = mean(cssmean, na.rm = TRUE),
            n = n(), .groups = "drop")

p7 <- ggplot(heatmap_data, aes(x = decade, y = poly_quartile, fill = mean_css)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f\n(n=%d)", mean_css, n)), size = 3.5) +
  scale_fill_gradient2(low = "#D6604D", mid = "#F7F7F7", high = "#2166AC",
                       midpoint = median(heatmap_data$mean_css)) +
  labs(x = "Decade", y = "Polyarchy quartile", fill = "Mean\nCSS",
       title = "Social Science Output: Polyarchy Quartile by Decade",
       subtitle = "Cell values show mean CSSmean and observation count") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank())

ggsave("Figures/fig7_heatmap_polyarchy_decade.png", p7, width = 7, height = 5, dpi = 300)
cat("  Saved: Figures/fig7_heatmap_polyarchy_decade.png\n")

# ============================================================================
# TABLE 1 (LaTeX): Descriptive statistics by regime type
# ============================================================================
cat("\n--- Table 1: Descriptive statistics by regime type ---\n")

desc_regime <- data %>%
  filter(!is.na(regime_label)) %>%
  group_by(regime_label) %>%
  summarise(N = n(),
            Countries = n_distinct(country),
            Mean = mean(cssmean, na.rm = TRUE),
            SD = sd(cssmean, na.rm = TRUE),
            Min = min(cssmean, na.rm = TRUE),
            Median = median(cssmean, na.rm = TRUE),
            Max = max(cssmean, na.rm = TRUE),
            Mean_Polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
            .groups = "drop")

# Print to console
print(as.data.frame(desc_regime))

# Write LaTeX
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\caption{\\label{tab:desc_regime} Descriptive Statistics: CSS Mean by Regime Type}",
  "\\bigskip",
  "\\centering",
  "\\begin{tabular}{lrrrrrrr}",
  "\\toprule",
  "Regime type & N & Countries & Mean & SD & Min & Median & Max \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(desc_regime))) {
  r <- desc_regime[i, ]
  lab <- gsub("\n", " ", as.character(r$regime_label))
  tex_lines <- c(tex_lines,
    sprintf("%s & %d & %d & %.2f & %.2f & %.2f & %.2f & %.2f \\\\",
            lab, r$N, r$Countries, r$Mean, r$SD, r$Min, r$Median, r$Max))
}
tex_lines <- c(tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par \\raggedright",
  "Regime classification based on V-DEM \\texttt{v2x\\_regime}: 0 = closed autocracy, 1 = electoral autocracy, 2 = electoral democracy, 3 = liberal democracy.",
  "\\end{table}"
)
writeLines(tex_lines, "Tables/Desc_Regime.tex")
cat("  Saved: Tables/Desc_Regime.tex\n")

# ============================================================================
# TABLE 2 (LaTeX): Descriptive statistics by polyarchy quartile
# ============================================================================
cat("--- Table 2: Descriptive statistics by polyarchy quartile ---\n")

desc_poly <- data %>%
  filter(!is.na(v2x_polyarchy) & !is.na(cssmean)) %>%
  mutate(poly_quartile = cut(v2x_polyarchy,
                             breaks = quantile(v2x_polyarchy, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                             include.lowest = TRUE,
                             labels = c("Q1 (0.01-0.32)", "Q2 (0.32-0.60)",
                                        "Q3 (0.60-0.84)", "Q4 (0.84-0.92)"))) %>%
  group_by(poly_quartile) %>%
  summarise(N = n(),
            Countries = n_distinct(country),
            Mean_CSS = mean(cssmean, na.rm = TRUE),
            SD_CSS = sd(cssmean, na.rm = TRUE),
            Mean_Polyarchy = mean(v2x_polyarchy, na.rm = TRUE),
            Pct_Autocracy = 100 * mean(autocracy, na.rm = TRUE),
            .groups = "drop")

print(as.data.frame(desc_poly))

tex_lines2 <- c(
  "\\begin{table}[htbp]",
  "\\caption{\\label{tab:desc_polyarchy} Descriptive Statistics: CSS Mean by Polyarchy Quartile}",
  "\\bigskip",
  "\\centering",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "Polyarchy quartile & N & Countries & Mean CSS & SD & Mean polyarchy & \\% Autocracy \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(desc_poly))) {
  r <- desc_poly[i, ]
  tex_lines2 <- c(tex_lines2,
    sprintf("%s & %d & %d & %.2f & %.2f & %.3f & %.1f\\%% \\\\",
            as.character(r$poly_quartile), r$N, r$Countries,
            r$Mean_CSS, r$SD_CSS, r$Mean_Polyarchy, r$Pct_Autocracy))
}
tex_lines2 <- c(tex_lines2,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par \\raggedright",
  "Quartiles based on the pooled distribution of \\texttt{v2x\\_polyarchy}. Autocracy = 1 if \\texttt{v2x\\_regime} $<$ 2.",
  "\\end{table}"
)
writeLines(tex_lines2, "Tables/Desc_Polyarchy_Quartile.tex")
cat("  Saved: Tables/Desc_Polyarchy_Quartile.tex\n")

# ============================================================================
# TABLE 3 (LaTeX): Correlation matrix
# ============================================================================
cat("--- Table 3: Correlation matrix ---\n")

cor_vars <- c("cssmean", "v2x_polyarchy", "e_gdppc", "e_pop", "autocracy")
cor_labels <- c("CSS Mean", "Polyarchy", "GDP p.c.", "Population", "Autocracy")
cor_data <- data[, cor_vars]
cor_mat <- cor(cor_data, use = "pairwise.complete.obs")

# Print
cat("\nCorrelation matrix:\n")
print(round(cor_mat, 3))

tex_lines3 <- c(
  "\\begin{table}[htbp]",
  "\\caption{\\label{tab:corr_matrix} Pairwise Correlations}",
  "\\bigskip",
  "\\centering",
  paste0("\\begin{tabular}{l", paste(rep("r", length(cor_labels)), collapse = ""), "}"),
  "\\toprule",
  paste0(" & ", paste(cor_labels, collapse = " & "), " \\\\"),
  "\\midrule"
)
for (i in seq_along(cor_labels)) {
  vals <- sapply(seq_along(cor_labels), function(j) {
    if (j < i) sprintf("%.3f", cor_mat[i, j])
    else if (j == i) "1"
    else ""
  })
  tex_lines3 <- c(tex_lines3,
    paste0(cor_labels[i], " & ", paste(vals, collapse = " & "), " \\\\"))
}
tex_lines3 <- c(tex_lines3,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par \\raggedright",
  "Pairwise Pearson correlations. GDP and population are in levels (not logged).",
  "\\end{table}"
)
writeLines(tex_lines3, "Tables/Correlation_Matrix.tex")
cat("  Saved: Tables/Correlation_Matrix.tex\n")

cat("\n=== Step 6 complete: 7 figures, 3 tables ===\n")
