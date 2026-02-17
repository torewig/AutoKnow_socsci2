

# Load required packages (install if missing) and read data
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
library(ggplot2)
library(countrycode)

df <- read.csv("Data/cntry_avgs.csv", stringsAsFactors = FALSE)
head(df)

# Basic checks
if (!("cssmean" %in% names(df))) stop("Column 'cssmean' not found in Data/cntry_avgs.csv")
if (!("country" %in% names(df))) stop("Column 'country' not found in Data/cntry_avgs.csv")

# --- Add V-DEM country codes ---
# Map country names to V-DEM country codes using countrycode package
df$vdem_code <- countrycode(df$country, 
                            origin = "country.name", 
                            destination = "vdem",
                            warn = FALSE)

# Display mapping summary
cat("Country to V-DEM code mapping:\n")
cat("Countries successfully mapped:", sum(!is.na(df$vdem_code)), "\n")
cat("Countries with missing codes:", sum(is.na(df$vdem_code)), "\n")
if (sum(is.na(df$vdem_code)) > 0) {
  cat("Countries with missing codes:\n")
  print(df$country[is.na(df$vdem_code)])
}
cat("\n")

# --- List countries that did not get a V-DEM country code ---
unmapped_countries <- unique(df$country[is.na(df$vdem_code)])
if (length(unmapped_countries) > 0) {
  cat("UNMAPPED COUNTRIES (", length(unmapped_countries), " total):\n", sep = "")
  unmapped_df <- data.frame(country = unmapped_countries)
  print(unmapped_df)
  
  # Save unmapped countries to a file for manual review
  write.csv(unmapped_df, "Data/unmapped_countries.csv", row.names = FALSE)
  cat("\nUnmapped countries saved to: Data/unmapped_countries.csv\n")
  cat("You may need to manually correct country names or add custom mappings.\n\n")
} else {
  cat("All countries successfully mapped to V-DEM codes!\n\n")
}

# Order countries by descending cssmean and make 'country' an ordered factor
df <- df[order(-df$cssmean), ]
df$country <- factor(df$country, levels = unique(df$country))

# Create the bar plot (ranked highest to lowest) and display it
p <- ggplot(df, aes(x = country, y = cssmean)) +
	geom_col(fill = "steelblue") +
	coord_flip() +
	labs(x = "Country", y = "CSS Mean", title = "CSS Mean by country (ranked highest to lowest)") +
	theme_minimal(base_size = 12)

print(p)

# Save plot to file in working directory
ggsave("cssmean_by_country.png", plot = p, width = 8, height = 6, dpi = 300)

