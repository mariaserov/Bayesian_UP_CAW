# Create Table 1: Descriptive Statistics for Dowry Deaths Analysis

# Set working directory to the folder in which this script sits (so the html output also appears next to this script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(knitr)
library(kableExtra)

# Calculate overall summary statistics
total_dowry_deaths <- sum(data$dowry)
mean_yearly_dowry_deaths <- mean(yearly_dowry$total_observed_dd)
sd_yearly_dowry_deaths <- sd(yearly_dowry$total_observed_dd)
total_expected_deaths <- sum(data$e_dowry)
overall_smr <- total_dowry_deaths / total_expected_deaths
mean_district_smr <- mean(data$smr_dowry)
sd_district_smr <- sd(data$smr_dowry)
median_district_smr <- median(data$smr_dowry)
min_district_smr <- min(data$smr_dowry)
max_district_smr <- max(data$smr_dowry)

# Calculate district-level statistics
district_stats <- data %>%
  group_by(dist) %>%
  summarize(
    total_deaths = sum(dowry),
    mean_yearly_deaths = mean(dowry),
    mean_smr = mean(smr_dowry),
    .groups = 'drop'
  )

# Number of districts with SMR > 1
districts_high_risk <- sum(district_stats$mean_smr > 1)
districts_high_risk_pct <- (districts_high_risk / nrow(district_stats)) * 100

# Calculate yearly statistics
yearly_stats <- data %>%
  group_by(year) %>%
  summarize(
    total_deaths = sum(dowry),
    mean_deaths_per_district = mean(dowry),
    mean_smr = mean(smr_dowry),
    .groups = 'drop'
  )

# Year with highest and lowest deaths
year_max_deaths <- yearly_stats$year[which.max(yearly_stats$total_deaths)]
year_min_deaths <- yearly_stats$year[which.min(yearly_stats$total_deaths)]

# Calculate top 5 districts with highest mean SMR
top_districts <- district_stats %>%
  arrange(desc(mean_smr)) %>%
  head(5)

# Calculate bottom 5 districts with lowest mean SMR
bottom_districts <- district_stats %>%
  arrange(mean_smr) %>%
  head(5)

# Create Table 1
table1_dowry <- data.frame(
  Category = c(
    "Overall Statistics",
    "Total observed dowry deaths (2001-2014)",
    "Mean yearly dowry deaths (SD)",
    "Total expected dowry deaths (2001-2014)",
    "Overall SMR (observed/expected)",
    "Mean district-level SMR (SD)",
    "Median district-level SMR",
    "Range of district-level SMR",
    "Districts with excess risk (SMR > 1)",
    "",
    "Temporal Patterns",
    "Year with highest dowry deaths",
    "Year with lowest dowry deaths",
    "Range of yearly total deaths",
    "",
    "Spatial Patterns",
    "Districts with highest risk (Mean SMR)",
    paste("1.", top_districts$dist[1]),
    paste("2.", top_districts$dist[2]),
    paste("3.", top_districts$dist[3]),
    paste("4.", top_districts$dist[4]),
    paste("5.", top_districts$dist[5]),
    "",
    "Districts with lowest risk (Mean SMR)",
    paste("1.", bottom_districts$dist[1]),
    paste("2.", bottom_districts$dist[2]),
    paste("3.", bottom_districts$dist[3]),
    paste("4.", bottom_districts$dist[4]),
    paste("5.", bottom_districts$dist[5])
  ),
  
  Value = c(
    "",
    format(total_dowry_deaths, big.mark=","),
    sprintf("%.1f (%.1f)", mean_yearly_dowry_deaths, sd_yearly_dowry_deaths),
    format(round(total_expected_deaths), big.mark=","),
    sprintf("%.3f", overall_smr),
    sprintf("%.3f (%.3f)", mean_district_smr, sd_district_smr),
    sprintf("%.3f", median_district_smr),
    sprintf("%.3f - %.3f", min_district_smr, max_district_smr),
    sprintf("%d (%.1f%%)", districts_high_risk, districts_high_risk_pct),
    "",
    "",
    sprintf("%d (%d deaths)", year_max_deaths, max(yearly_stats$total_deaths)),
    sprintf("%d (%d deaths)", year_min_deaths, min(yearly_stats$total_deaths)),
    sprintf("%d - %d", min(yearly_stats$total_deaths), max(yearly_stats$total_deaths)),
    "",
    "",
    "Mean SMR",
    sprintf("%.3f", top_districts$mean_smr[1]),
    sprintf("%.3f", top_districts$mean_smr[2]),
    sprintf("%.3f", top_districts$mean_smr[3]),
    sprintf("%.3f", top_districts$mean_smr[4]),
    sprintf("%.3f", top_districts$mean_smr[5]),
    "",
    "Mean SMR",
    sprintf("%.3f", bottom_districts$mean_smr[1]),
    sprintf("%.3f", bottom_districts$mean_smr[2]),
    sprintf("%.3f", bottom_districts$mean_smr[3]),
    sprintf("%.3f", bottom_districts$mean_smr[4]),
    sprintf("%.3f", bottom_districts$mean_smr[5])
  )
)

# Format the table for display
kable(table1_dowry, 
      col.names = c("Characteristic", "Value"),
      caption = "Table 1: Descriptive statistics for dowry deaths in Uttar Pradesh (2001-2014)",
      align = c('l', 'r')) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(c(1, 10, 11, 15, 16, 23), bold = TRUE, background = "#f8f8f8") %>%
  footnote(general = "SMR = Standardized Mortality Ratio; SD = Standard Deviation", 
           footnote_as_chunk = TRUE, 
           general_title = "Note: ")