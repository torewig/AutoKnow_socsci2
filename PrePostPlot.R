# ============================================================================
# PrePostPlot.R
# Pre-post plot: average cssmean 5 years before and after transition to autocracy
# ============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Load operationalized data
input_path <- "Data/cy_operationalized.rds"
data <- readRDS(input_path)

# Only keep countries with year, autocracy, and cssmean
if (!all(c("country", "year", "autocracy", "cssmean") %in% names(data))) {
  stop("Data must contain 'country', 'year', 'autocracy', and 'cssmean' columns.")
}

# Sort data for lag/lead operations
library(dplyr)
data <- data %>% arrange(country, year)

# Identify transitions to autocracy (autocracy changes from 0 to 1)
data <- data %>%
  group_by(country) %>%
  mutate(autocracy_lag = lag(autocracy),
         transition = ifelse(!is.na(autocracy_lag) & autocracy_lag == 0 & autocracy == 1, 1, 0)) %>%
  ungroup()

# For each transition, collect cssmean for -5 to +5 years around transition year
prepost_list <- list()
for (i in which(data$transition == 1)) {
  ctry <- data$country[i]
  t_year <- data$year[i]
  window <- data %>% filter(country == ctry & year >= (t_year - 5) & year <= (t_year + 5)) %>%
    mutate(rel_year = year - t_year)
  window$transition_country <- ctry
  window$transition_year <- t_year
  prepost_list[[length(prepost_list) + 1]] <- window
}

prepost_data <- bind_rows(prepost_list)

# Compute average cssmean by relative year
avg_cssmean <- prepost_data %>%
  group_by(rel_year) %>%
  summarise(mean_cssmean = mean(cssmean, na.rm = TRUE), n = n())

# Plot
p <- ggplot(avg_cssmean, aes(x = rel_year, y = mean_cssmean)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 2, color = "darkblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Average CSS Mean: 5 Years Before and After Transition to Autocracy",
       x = "Years from Transition (0 = year of transition)",
       y = "Average CSS Mean") +
  theme_minimal(base_size = 14)

print(p)
ggsave("prepost_cssmean_autocracy.png", plot = p, width = 8, height = 5, dpi = 300)
cat("Pre-post plot saved as prepost_cssmean_autocracy.png\n")
