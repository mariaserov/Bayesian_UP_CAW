# Descriptive Analysis of Dowry Deaths in Uttar Pradesh

library(dplyr)        # For data manipulation
library(sf)           # For spatial data processing
library(spdep)        # For spatial weight matrices
library(ggplot2)      # For visualization
library(RColorBrewer) # For color palettes
library(knitr)        # For tables
library(kableExtra)   # For enhanced tables
library(INLA)         # For Bayesian modeling
library(tidyr)        # For data reshaping

# Assume "data" is already loaded from
# load("../DS6_CrimeUttarPradesh/CrimeUttarPradesh.RData")

# 1. Temporal analysis of dowry deaths

# Calculate yearly totals
yearly_dowry <- data %>%
  group_by(year) %>%
  summarize(
    total_observed_dd = sum(dowry),
    total_expected_dd = sum(e_dowry),
    yearly_total_smr_dd = sum(dowry) / sum(e_dowry),
    averaged_indv_smr_dd = mean(smr_dowry),
    .groups = 'drop'
  )

# Print the yearly summary
print(yearly_dowry)

# Plot yearly trends with two SMR lines
p_yearly_trend <- ggplot(yearly_dowry, aes(x = year)) +
  geom_line(aes(y = yearly_total_smr_dd, colour = "Total SMR"), size = 1) +
  geom_line(aes(y = averaged_indv_smr_dd, colour = "Average Individual SMR"), size = 1, linetype = "dotted") +
  geom_point(aes(y = yearly_total_smr_dd, colour = "Total SMR"), size = 3) +
  geom_point(aes(y = averaged_indv_smr_dd, colour = "Average Individual SMR"), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  scale_colour_manual(values = c("Total SMR" = "blue", "Average Individual SMR" = "green")) +
  scale_x_continuous(breaks = seq(min(yearly_dowry$year), max(yearly_dowry$year), by = 2)) +
  theme_minimal() +
  labs(
    title = "Yearly SMR for Dowry Deaths",
    x = "Year",
    y = "SMR",
    colour = "SMR Type",
    caption = "Red dashed line = SMR of 1 (observed = expected)"
  )

print(p_yearly_trend)


# 2. District-level analysis

# Calculate district-level averages across all years
district_dowry <- data %>%
  group_by(dist, ID_area) %>%
  summarize(
    total_observed_dd = sum(dowry),
    total_expected = sum(e_dowry),
    average_smr = mean(smr_dowry),
    .groups = 'drop'
  ) %>%
  arrange(desc(average_smr))

# View top and bottom 10 districts by average SMR
top10_districts <- head(district_dowry, 10)
bottom10_districts <- tail(district_dowry, 10)

print("Top 10 districts by average SMR:")
print(top10_districts)

print("Bottom 10 districts by average SMR:")
print(bottom10_districts)

# 3. Spatial visualization

# Create categories for SMR values
smr_breaks <- c(0, 0.5, 0.8, 1, 1.2, 1.5, 2, max(district_dowry$average_smr))
district_dowry$smr_category <- cut(
  district_dowry$average_smr, 
  breaks = smr_breaks,
  labels = c("<0.5", "0.5-0.8", "0.8-1", "1-1.2", "1.2-1.5", "1.5-2", ">2"),
  include.lowest = TRUE
)

# Merge the aggregated data with the spatial polygons
carto_up_dowry <- carto_up
carto_up_dowry@data <- merge(carto_up_dowry@data, district_dowry, by = "ID_area", all.x = TRUE)

# Convert to sf object for nicer plotting with ggplot2
carto_up_sf <- st_as_sf(carto_up_dowry)

# Create a choropleth map of average SMR
p_dowry_map <- ggplot() +
  geom_sf(data = carto_up_sf, aes(fill = smr_category), color = "black", size = 0.1) +
  scale_fill_brewer(palette = "OrRd", name = "SMR (Dowry Deaths)") +
  theme_minimal() +
  labs(
    title = "Average Standardized Mortality Ratio for Dowry Deaths",
    subtitle = "Uttar Pradesh Districts (2001-2014)",
    caption = "Data Source: National Crime Records Bureau"
  )

print(p_dowry_map)

# 4. Testing for spatial autocorrelation

# Create a list of neighbors
nb <- poly2nb(carto_up, queen = TRUE)
# Convert to weights list
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate Moran's I for average SMR (check for spatial autocorrelation)
moran_result <- moran.test(carto_up_dowry@data$average_smr, lw, zero.policy = TRUE)
print(moran_result)

# Moran I test under randomisation
# 
# data:  carto_up_dowry@data$average_smr  
# weights: lw    
# 
# Moran I statistic standard deviate = 9.6374, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.735829482      -0.014492754       0.006061398 

# Create a Moran scatterplot
moran_plot <- moran.plot(carto_up_dowry@data$average_smr, lw, zero.policy = TRUE,
                         labels = as.character(carto_up_dowry@data$dist), 
                         xlab = "Standardized Dowry Death SMR", 
                         ylab = "Spatially Lagged SMR")

# Interpretation: Moran’s I statistic = 0.736 (high-ish) = positive spatial autocorrelation — districts with high SMR tend to be near other high SMR districts (same for low). If no spatial correlation, this would be -0.014492754. p-value < 2.2e-16 = significant. Spatial clustering is not due to chance. There is clear spatial clustering: dowry deaths tend to be geographically concentrated.

#Graph: Spatially lagged SMR = average SMR of neighbouring districts. Upwards sloping line shows that districts with high SMRs tend to be near other districts with high SMRs (similar averaged neighbouring SMRs). Etah and Mainpuri are high. Mathura and Hardoi have lower SMRs than their neighbours?

# 5. Identifying spatio-temporal patterns

# Create a year-district matrix of SMR values for exploration
smr_matrix <- data %>%
  select(ID_area, year, smr_dowry) %>%
  pivot_wider(
    names_from = year,
    values_from = smr_dowry,
    id_cols = ID_area
  )

# Calculate correlation between consecutive years to assess temporal stability
year_cors <- numeric(13)
for (i in 1:13) {
  year_cors[i] <- cor(data$smr_dowry[data$year == 2000 + i],
                      data$smr_dowry[data$year == 2001 + i],
                      use = "pairwise.complete.obs")
}

year_cors_df <- data.frame(
  year_pair = paste(2001:(2014-1), 2002:2014, sep="-"),
  correlation = year_cors
)

print("Correlation between consecutive years:")
print(year_cors_df)
# Ben: Why does 2007-2008 (0.6874903) have the lowest correlation?

# Identify districts with consistently high SMR (>1.5) across years
high_risk_districts <- data %>%
  group_by(dist, ID_area) %>%
  summarize(
    high_risk_years = sum(smr_dowry > 1.5),
    avg_smr = mean(smr_dowry),
    .groups = 'drop'
  ) %>%
  filter(high_risk_years >= 7) %>%
  arrange(desc(high_risk_years))

print("Districts with consistently high dowry death SMR:")
print(high_risk_districts)

# 6. Space-time visualization

# Create a heatmap of SMR values by district and year
# Limit to the top 20 districts by average SMR for readability
top_districts <- district_dowry %>%
  top_n(20, average_smr) %>%
  pull(dist)

district_year_smr <- data %>%
  filter(dist %in% top_districts) %>%
  mutate(
    year = as.factor(year),
    # Reorder districts by their average SMR for better visualization
    dist = factor(dist, levels = top_districts)
  )

# Create the heatmap
p_heatmap <- ggplot(district_year_smr, aes(x = year, y = dist, fill = smr_dowry)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 1,
    name = "SMR"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Spatio-temporal Pattern of Dowry Deaths",
    subtitle = "Top 20 districts by average SMR (2001-2014)",
    x = "Year",
    y = "District"
  )

print(p_heatmap)

# 8. Summary statistics for report

cat("\nSummary of Dowry Deaths Analysis:\n")
cat("Total dowry deaths over study period:", sum(data$dowry), "\n")
cat("Average yearly dowry deaths:", mean(yearly_dowry$total_observed_dd), "\n")
cat("Average SMR across all districts and years:", mean(data$smr_dowry), "\n")
cat("Moran's I statistic:", round(moran_result$estimate[1], 3), 
    "p-value:", round(moran_result$p.value, 5), "\n")
cat("Number of high-risk districts (SMR > 1.5 in ≥7 years):", nrow(high_risk_districts), "\n")
cat("District with highest average SMR:", as.character(district_dowry$dist[1]), 
    "SMR =", round(district_dowry$average_smr[1], 2), "\n")
cat("Years with highest SMR:", yearly_dowry$year[which.max(yearly_dowry$yearly_total_smr_dd)], 
    "SMR =", round(max(yearly_dowry$yearly_total_smr_dd), 2), "\n")
cat("Average year-to-year correlation of SMR:", round(mean(year_cors), 3), "\n")