# ============================================================================
# Explore_SynthControl_Candidates.R
# Identify suitable cases for synthetic control (CSSmean ~ autocracy)
# ============================================================================

# Load operationalized data
input_path <- "Data/cy_operationalized.rds"
data <- readRDS(input_path)

# Only keep countries with year, autocracy, and cssmean
if (!all(c("country", "year", "autocracy", "cssmean") %in% names(data))) {
  stop("Data must contain 'country', 'year', 'autocracy', and 'cssmean' columns.")
}

library(dplyr)
data <- data %>% arrange(country, year)

# Identify transitions to autocracy (autocracy changes from 0 to 1)
data <- data %>%
  group_by(country) %>%
  mutate(autocracy_lag = lag(autocracy),
         transition = ifelse(!is.na(autocracy_lag) & autocracy_lag == 0 & autocracy == 1, 1, 0)) %>%
  ungroup()

# For each transition, check for at least 5 years of pre- and post-treatment data with non-missing cssmean
good_cases <- data.frame()
for (i in which(data$transition == 1)) {
  ctry <- data$country[i]
  t_year <- data$year[i]
  window <- data %>% filter(country == ctry & year >= (t_year - 5) & year <= (t_year + 5))
  n_pre <- sum(window$year < t_year & !is.na(window$cssmean))
  n_post <- sum(window$year > t_year & !is.na(window$cssmean))
  if (n_pre >= 3 & n_post >= 3) { # at least 3 years pre and post
    good_cases <- rbind(good_cases, data.frame(country = ctry, transition_year = t_year, n_pre = n_pre, n_post = n_post))
  }
}

if (nrow(good_cases) > 0) {
  cat("\nSuitable synthetic control cases (at least 3 years pre and post):\n")
  print(good_cases)
} else {
  cat("\nNo suitable synthetic control cases found with at least 3 years pre and post data.\n")
}
