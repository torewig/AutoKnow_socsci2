# ============================================================================
# Step1_LoadAndMap.R
# Load cntryyear_avgs.csv and map V-DEM country codes using countrycode
# ============================================================================
rm(list = ls())

if (!requireNamespace("countrycode", quietly = TRUE)) install.packages("countrycode")
library(countrycode)

# --- Load data ---
cy_path <- "Data/cntryyear_avgs.csv"
if (!file.exists(cy_path)) stop(paste0("File not found: ", cy_path))

cy <- read.csv(cy_path, stringsAsFactors = FALSE)
cat("Loaded:", cy_path, "\n")
cat("Dimensions:", nrow(cy), "rows x", ncol(cy), "cols\n")
cat("Columns:", paste(names(cy), collapse = ", "), "\n\n")

# --- Quick look at cssmean ---
cat("=== cssmean summary ===\n")
print(summary(cy$cssmean))
cat("Non-missing cssmean:", sum(!is.na(cy$cssmean)), "/", nrow(cy), "\n\n")

# --- Map country names to V-DEM numeric codes ---
cy$vdem_code <- countrycode(cy$country,
                            origin = "country.name",
                            destination = "vdem",
                            warn = FALSE)

cat("V-DEM code mapping:\n")
cat("  Mapped:", sum(!is.na(cy$vdem_code)), "rows\n")
cat("  Unmapped:", sum(is.na(cy$vdem_code)), "rows\n\n")

# List unmapped countries
unmapped <- unique(cy$country[is.na(cy$vdem_code)])
if (length(unmapped) > 0) {
  cat("Unmapped countries (", length(unmapped), "):\n", sep = "")
  for (u in unmapped) cat("  -", u, "\n")
  cat("\n")
}

# --- Save intermediate result ---
out_path <- "Data/cy_step1_mapped.rds"
saveRDS(cy, out_path)
cat("Saved mapped data to:", out_path, "\n")
cat("Done.\n")
