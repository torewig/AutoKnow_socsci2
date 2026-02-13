# ============================================================================
# Datacreation2.R
# Load country-year averaged data and map V-DEM codes to country names
# ============================================================================
rm(list = ls())
# Load required packages (install if missing)
if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
library(countrycode)

# Load country-year averaged data
cy_path <- "Data/cntryyear_avgs.csv"
if (!file.exists(cy_path)) {
  stop(paste0("File not found: ", cy_path))
}

cy <- read.csv(cy_path, stringsAsFactors = FALSE)
cat("Data loaded from", cy_path, "\n")
cat("Dimensions:", nrow(cy), "rows,", ncol(cy), "columns\n")
cat("Columns:", paste(names(cy), collapse = ", "), "\n\n")

# Basic checks
if (!("country" %in% names(cy))) {
  stop("Column 'country' not found in Data/cntryyear_avgs.csv")
}

# --- Map V-DEM codes to country names ---
# Use countrycode to map country names to V-DEM codes
cy$vdem_code <- countrycode(cy$country, 
                            origin = "country.name", 
                            destination = "vdem",
                            warn = FALSE)

cat("V-DEM code mapping summary:\n")
cat("  Total rows:", nrow(cy), "\n")
cat("  Countries successfully mapped:", sum(!is.na(cy$vdem_code)), "rows\n")
cat("  Rows with missing V-DEM codes:", sum(is.na(cy$vdem_code)), "\n")
cat("\n")

# --- List unmapped countries ---
unmapped_countries <- unique(cy$country[is.na(cy$vdem_code)])
if (length(unmapped_countries) > 0) {
  cat("UNMAPPED COUNTRIES (", length(unmapped_countries), " total):\n", sep = "")
  for (i in seq_along(unmapped_countries)) {
    cat("  ", i, ". ", unmapped_countries[i], "\n", sep = "")
  }
  cat("\n")
  
  # Save unmapped countries to file for manual review
  unmapped_df <- data.frame(country = unmapped_countries)
  write.csv(unmapped_df, "Data/unmapped_countries_cy.csv", row.names = FALSE)
  cat("Unmapped countries saved to: Data/unmapped_countries_cy.csv\n\n")
} else {
  cat("All countries successfully mapped to V-DEM codes!\n\n")
}

# --- Show sample of mapped data ---
cat("Sample of mapped data (first 10 rows):\n")
print(head(cy[, c("country", "year", "vdem_code", "cssmean")], 10))
cat("\n")

# Display unique V-DEM codes
cat("Unique V-DEM codes in data (", length(unique(cy$vdem_code[!is.na(cy$vdem_code)])), " total):\n", sep = "")
print(sort(unique(cy$vdem_code[!is.na(cy$vdem_code)])))

# ============================================================================
# --- Load V-DEM data and merge on v2x_polyarchy ---
# ============================================================================

vdem_path <- "Data/V-Dem-CY-Full+Others-v15.rds"
if (!file.exists(vdem_path)) {
  stop(paste0("V-DEM file not found: ", vdem_path))
}

# Load V-DEM dataset
vdem <- readRDS(vdem_path)
cat("\nV-DEM dataset loaded from", vdem_path, "\n")
cat("Dimensions:", nrow(vdem), "rows,", ncol(vdem), "columns\n")

# Check for required columns
required_vdem_cols <- c("country_id", "year", "v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")
if (!all(required_vdem_cols %in% names(vdem))) {
  missing_cols <- setdiff(required_vdem_cols, names(vdem))
  stop(paste("Required V-DEM columns not found:", paste(missing_cols, collapse = ", ")))
}

# Subset V-DEM to keep only the required columns
vdem_subset <- vdem[, required_vdem_cols]
cat("V-DEM subset: kept columns ", paste(required_vdem_cols, collapse=", "), "\n")
cat("V-DEM subset dimensions:", nrow(vdem_subset), "rows\n\n")

# Merge cy with vdem_subset on vdem_code (from cy) and country_id (from vdem)
# Also match on year
cy_merged <- merge(cy, vdem_subset,
                   by.x = c("vdem_code", "year"),
                   by.y = c("country_id", "year"),
                   all.x = TRUE)

cat("Merge results:\n")
cat("  Original cy rows:", nrow(cy), "\n")
cat("  Merged rows:", nrow(cy_merged), "\n")
cat("  Rows with v2x_polyarchy data:", sum(!is.na(cy_merged$v2x_polyarchy)), "\n")
cat("  Rows missing v2x_polyarchy:", sum(is.na(cy_merged$v2x_polyarchy)), "\n\n")

# Show sample of merged data
cat("Sample of merged data (first 10 rows with V-DEM variables):\n")
sample_rows <- head(cy_merged[, c("country", "year", "vdem_code", "cssmean", "v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")], 10)
print(sample_rows)
cat("\n")

# Summary statistics for V-DEM variables
cat("Summary statistics for v2x_polyarchy, e_gdppc, v2x_regime, e_pop:\n")
print(summary(cy_merged[, c("v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")]))

# --- Save final merged data as R dataset ---
final_rds_path <- "Data/cy_merged_final.rds"
saveRDS(cy_merged, final_rds_path)
cat("Final merged data saved as:", final_rds_path, "\n")

