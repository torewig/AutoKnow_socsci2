# ============================================================================
# Step2_MergeVDEM.R
# Merge country-year data with V-DEM for: v2x_polyarchy, e_gdppc, v2x_regime, e_pop
# Uses vdem_code (mapped in Step 1) as bridge to V-DEM country_id
# ============================================================================
rm(list = ls())

# --- Load Step 1 output ---
cy_path <- "Data/cy_step1_mapped.rds"
if (!file.exists(cy_path)) stop(paste0("Run Step1_LoadAndMap.R first. File not found: ", cy_path))
cy <- readRDS(cy_path)
cat("Loaded:", cy_path, "(", nrow(cy), "rows )\n\n")

# --- Load V-DEM dataset ---
vdem_path <- "Data/V-Dem-CY-Full+Others-v15.rds"
if (!file.exists(vdem_path)) stop(paste0("V-DEM file not found: ", vdem_path))
vdem <- readRDS(vdem_path)
cat("Loaded V-DEM:", vdem_path, "(", nrow(vdem), "rows x", ncol(vdem), "cols )\n")

# --- Select only the requested V-DEM variables ---
vdem_vars <- c("country_id", "year", "v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")
missing_vars <- setdiff(vdem_vars, names(vdem))
if (length(missing_vars) > 0) stop(paste("V-DEM columns not found:", paste(missing_vars, collapse = ", ")))

vdem_sub <- vdem[, vdem_vars]
cat("V-DEM subset: kept", paste(vdem_vars, collapse = ", "), "\n\n")

# --- Merge on vdem_code = country_id + year ---
cy_merged <- merge(cy, vdem_sub,
                   by.x = c("vdem_code", "year"),
                   by.y = c("country_id", "year"),
                   all.x = TRUE)

cat("=== Merge results ===\n")
cat("  Input rows:", nrow(cy), "\n")
cat("  Merged rows:", nrow(cy_merged), "\n")
cat("  With v2x_polyarchy:", sum(!is.na(cy_merged$v2x_polyarchy)), "\n")
cat("  With e_gdppc:", sum(!is.na(cy_merged$e_gdppc)), "\n")
cat("  With v2x_regime:", sum(!is.na(cy_merged$v2x_regime)), "\n")
cat("  With e_pop:", sum(!is.na(cy_merged$e_pop)), "\n\n")

# --- Show sample ---
cat("Sample of merged data:\n")
print(head(cy_merged[, c("country", "year", "cssmean", "v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")], 15))
cat("\n")

# --- Summary of merged V-DEM variables ---
cat("=== Summary of V-DEM variables in merged data ===\n")
print(summary(cy_merged[, c("v2x_polyarchy", "e_gdppc", "v2x_regime", "e_pop")]))

# --- Save merged data ---
out_path <- "Data/cy_cssmean_vdem_merged.rds"
saveRDS(cy_merged, out_path)
cat("\nSaved merged data to:", out_path, "\n")

# Also save as CSV for convenience
csv_path <- "Data/cy_cssmean_vdem_merged.csv"
write.csv(cy_merged, csv_path, row.names = FALSE)
cat("Saved CSV to:", csv_path, "\n")
cat("Done.\n")
