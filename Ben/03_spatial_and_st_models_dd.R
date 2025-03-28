# Bayesian Spatial and Spatio-temporal Models for Dowry Deaths in Uttar Pradesh
# Following the template structure from the practical example

library(INLA)
library(ggplot2)
library(dplyr)
library(sf)
library(spdep)
library(RColorBrewer)
library(knitr)
library(kableExtra)

# load("../DS6_CrimeUttarPradesh/CrimeUttarPradesh.RData")

# First, create a spatial-only model using the BYM2 prior

# 1. Prepare data for spatial-only analysis (aggregating over time)
dowry_spatial_data <- data %>%
  group_by(dist, ID_area) %>%
  summarize(
    O = sum(dowry),
    E = sum(e_dowry),
    .groups = 'drop'
  )

# Create an adjacency structure for INLA if not already created
nb <- poly2nb(carto_up, queen = TRUE)
nb2INLA("UP_adj.graph", nb)
UP_adj <- paste(getwd(), "/UP_adj.graph", sep="")

# 2. Fit the BYM2 spatial model

# Define the PC priors for the BYM2 model
# The first prior is for the precision parameter (1/variance)
# The second prior is for the mixing parameter (phi)
formula_dowry_BYM2 <- O ~ 1 + 
  f(ID_area, model = "bym2", graph = UP_adj,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi = list(prior = "pc", param = c(0.5, 0.5))
    ))

# Fit the model
sBYM_dowry <- inla(formula = formula_dowry_BYM2, 
                   family = "poisson", 
                   data = dowry_spatial_data, 
                   E = E,
                   control.compute = list(dic = TRUE, waic = TRUE))

# 3. Extract posterior estimates
# Calculate relative risks (RR)
RR_sBYM_dowry <- numeric(70)  # 70 districts in Uttar Pradesh
for(i in 1:70) {
  RR_sBYM_dowry[i] <- inla.emarginal(function(x) exp(x), 
                                     sBYM_dowry$marginals.random$ID_area[[i]])
}

# Calculate posterior probabilities (PP) that RR > 1
RR_sBYM_marg_dowry <- sBYM_dowry$marginals.random$ID_area[1:70]
PP_sBYM_dowry <- lapply(RR_sBYM_marg_dowry, function(x) {1 - inla.pmarginal(0, x)})

# Create dataframe with results
resRR_PP_dowry <- data.frame(
  resRR = RR_sBYM_dowry,
  PP = unlist(PP_sBYM_dowry),
  ID_area = dowry_spatial_data$ID_area
)

# 4. Prepare for mapping
# Create categories for RR
resRR_PP_dowry$resRRcat <- cut(resRR_PP_dowry$resRR, 
                               breaks = c(min(resRR_PP_dowry$resRR), 
                                          0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 
                                          max(resRR_PP_dowry$resRR)),
                               include.lowest = TRUE)

# Create categories for PP
resRR_PP_dowry$PPcat <- cut(resRR_PP_dowry$PP, 
                            breaks = c(0, 0.2, 0.8, 1.00), 
                            include.lowest = TRUE)

# Join with spatial data
map_RR_PP_dowry <- merge(carto_up, resRR_PP_dowry, by = "ID_area", all.x = TRUE)
map_RR_PP_dowry_sf <- st_as_sf(map_RR_PP_dowry)

# 5. Create maps
p1_dowry <- ggplot() + 
  geom_sf(data = map_RR_PP_dowry_sf, aes(fill = resRRcat)) +
  theme_bw() + 
  scale_fill_brewer(palette = "PuOr") + 
  guides(fill = guide_legend(title = "RR")) + 
  ggtitle("RR Spatial model - Dowry Deaths") + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"))

p2_dowry <- ggplot() + 
  geom_sf(data = map_RR_PP_dowry_sf, aes(fill = PPcat)) +
  theme_bw() +
  scale_fill_viridis_d(
    option = "plasma", 
    name = "PP",
    direction = -1,
    guide = guide_legend(
      title.position = 'top',
      reverse = TRUE
    )) + 
  ggtitle("PP Spatial model - Dowry Deaths") + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"))

# Display the spatial model maps
p1_dowry | p2_dowry

# 6. Assess spatial fraction (Phi parameter)
# Extract the Phi hyperparameter to assess the strength of spatial clustering
phi_dowry <- sBYM_dowry$summary.hyperpar["Phi for ID_area", ]
print(phi_dowry)

# Now extend to spatio-temporal models

# 7. Prepare data for spatio-temporal analysis
dowry_st_data <- data
names(dowry_st_data)[c(5, 8)] <- c("O", "E")  # Rename columns for INLA format

# Create IDs for space and time
dowry_st_data$ID_space <- dowry_st_data$ID_area
dowry_st_data$ID_time <- dowry_st_data$ID_year  # Already 1:14 for years 2001-2014

# 8. Spatio-temporal model WITHOUT interaction
formula_st_dowry <- O ~ 1 + 
  f(ID_space, model = "bym2", graph = UP_adj,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi = list(prior = "pc", param = c(0.5, 0.5))
    )) + 
  f(ID_time, model = "rw1", 
    hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))

# Fit the spatio-temporal model without interaction
stBYM_dowry <- inla(formula = formula_st_dowry, 
                    family = "poisson", 
                    data = dowry_st_data, 
                    E = E,
                    control.compute = list(dic = TRUE, waic = TRUE))

# 9. Extract posterior means for spatial and temporal effects
# Spatial relative risks
RR_stBYM_dowry <- numeric(70)
for(i in 1:70) {
  RR_stBYM_dowry[i] <- inla.emarginal(function(x) exp(x), 
                                      stBYM_dowry$marginals.random$ID_space[[i]])
}

# Posterior probabilities (for spatial RR)
RR_stBYM_marg_dowry <- stBYM_dowry$marginals.random$ID_space[1:70]
PP_stBYM_dowry <- lapply(RR_stBYM_marg_dowry, function(x) {1 - inla.pmarginal(0, x)})

# Temporal relative risks and 95% CI
RR_stRW_RR_dowry <- numeric(14)
RR_stRW_lo_dowry <- numeric(14)
RR_stRW_hi_dowry <- numeric(14)

for(i in 1:14) {
  # Posterior mean
  RR_stRW_RR_dowry[i] <- inla.emarginal(function(x) exp(x), 
                                        stBYM_dowry$marginals.random$ID_time[[i]])
  # 2.5% quantile 
  RR_stRW_lo_dowry[i] <- inla.qmarginal(0.025, inla.tmarginal(function(x) exp(x), 
                                                              stBYM_dowry$marginals.random$ID_time[[i]]))
  # 97.5% quantile 
  RR_stRW_hi_dowry[i] <- inla.qmarginal(0.975, inla.tmarginal(function(x) exp(x), 
                                                              stBYM_dowry$marginals.random$ID_time[[i]]))
}

RR_stRW_dowry <- data.frame(
  RR = RR_stRW_RR_dowry,
  low = RR_stRW_lo_dowry,
  high = RR_stRW_hi_dowry,
  year = 2001:2014
)

# 10. Plot temporal trends
Temp1_dowry <- ggplot(RR_stRW_dowry, aes(x = year, y = RR)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
  ggtitle("ST model No Int - Dowry Deaths") + 
  labs(x = "Year", y = "Relative Risk") +
  theme_minimal() + scale_x_continuous(breaks = seq(min(RR_stRW_dowry$year), max(RR_stRW_dowry$year), by = 1))

print(Temp1_dowry)

# 11. Prepare for mapping spatial effects from ST model
resRR_PP_st_dowry <- data.frame(
  resRR = RR_stBYM_dowry,
  PP = unlist(PP_stBYM_dowry),
  ID_area = 1:70
)

# Create categories
resRR_PP_st_dowry$resRRcat <- cut(resRR_PP_st_dowry$resRR, 
                                  breaks = c(min(resRR_PP_st_dowry$resRR), 
                                             0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 
                                             max(resRR_PP_st_dowry$resRR)),
                                  include.lowest = TRUE)

resRR_PP_st_dowry$PPcat <- cut(resRR_PP_st_dowry$PP, 
                               breaks = c(0, 0.2, 0.8, 1.00), 
                               include.lowest = TRUE)

# Join with spatial data
map_RR_ST_dowry <- merge(carto_up, resRR_PP_st_dowry, by = "ID_area", all.x = TRUE)
map_RR_ST_dowry_sf <- st_as_sf(map_RR_ST_dowry)

# 12. Map spatial effects from ST model
p3_dowry <- ggplot() + 
  geom_sf(data = map_RR_ST_dowry_sf, aes(fill = resRRcat)) +
  theme_bw() + 
  scale_fill_brewer(palette = "PuOr") + 
  guides(fill = guide_legend(title = "RR")) + 
  ggtitle("RR ST model - Dowry Deaths") + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"))

p4_dowry <- ggplot() + 
  geom_sf(data = map_RR_ST_dowry_sf, aes(fill = PPcat)) +
  theme_bw() +
  scale_fill_viridis_d(
    option = "plasma", 
    name = "PP",
    direction = -1,
    guide = guide_legend(
      title.position = 'top',
      reverse = TRUE
    )) + 
  ggtitle("PP ST model - Dowry Deaths") + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"))

# 13. Spatio-temporal model WITH Type I interaction

# Create an ID for the space-time interaction
dowry_st_data$ID_space_time <- 1:nrow(dowry_st_data)

# Define the formula with interaction
formula_st_dowry_int <- O ~ 1 + 
  f(ID_space, model = "bym2", graph = UP_adj,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi = list(prior = "pc", param = c(0.5, 0.5))
    )) + 
  f(ID_time, model = "rw1", 
    hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))) +
  f(ID_space_time, model = "iid", 
    hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))

# Fit the model with interaction
stIntI_BYM_dowry <- inla(formula = formula_st_dowry_int, 
                         family = "poisson", 
                         data = dowry_st_data, 
                         E = E,
                         control.compute = list(dic = TRUE, waic = TRUE))

# 14. Extract posterior estimates from interaction model
# Spatial relative risks
RR_stIntI_BYM_dowry <- numeric(70)
for(i in 1:70) {
  RR_stIntI_BYM_dowry[i] <- inla.emarginal(function(x) exp(x), 
                                           stIntI_BYM_dowry$marginals.random$ID_space[[i]])
}

# Posterior probabilities (for spatial RR)
RR_stIntI_BYM_marg_dowry <- stIntI_BYM_dowry$marginals.random$ID_space[1:70]
PP_stIntI_BYM_dowry <- lapply(RR_stIntI_BYM_marg_dowry, function(x) {1 - inla.pmarginal(0, x)})

# Temporal relative risks and 95% CI
RR_stIntI_RW_RR_dowry <- numeric(14)
RR_stIntI_RW_lo_dowry <- numeric(14)
RR_stIntI_RW_hi_dowry <- numeric(14)

for(i in 1:14) {
  # Posterior mean
  RR_stIntI_RW_RR_dowry[i] <- inla.emarginal(function(x) exp(x), 
                                             stIntI_BYM_dowry$marginals.random$ID_time[[i]])
  # 2.5% quantile 
  RR_stIntI_RW_lo_dowry[i] <- inla.qmarginal(0.025, inla.tmarginal(function(x) exp(x), 
                                                                   stIntI_BYM_dowry$marginals.random$ID_time[[i]]))
  # 97.5% quantile 
  RR_stIntI_RW_hi_dowry[i] <- inla.qmarginal(0.975, inla.tmarginal(function(x) exp(x), 
                                                                   stIntI_BYM_dowry$marginals.random$ID_time[[i]]))
}

RR_stIntI_RW_dowry <- data.frame(
  RR = RR_stIntI_RW_RR_dowry,
  low = RR_stIntI_RW_lo_dowry,
  high = RR_stIntI_RW_hi_dowry,
  year = 2001:2014
)

# 15. Plot temporal trends for both models
Temp2_dowry <- ggplot(RR_stIntI_RW_dowry, aes(x = year, y = RR)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2) + 
  ggtitle("ST model With Int - Dowry Deaths") + 
  labs(x = "Year", y = "Relative Risk") +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(min(RR_stRW_dowry$year), max(RR_stRW_dowry$year), by = 1))

# Compare temporal trends side by side
temp_comparison_dowry <- Temp1_dowry | Temp2_dowry
print(temp_comparison_dowry)

##########
# Add model label to each dataset
RR_stRW_dowry$model <- "No Interaction"
RR_stIntI_RW_dowry$model <- "With Interaction"

# Combine the two datasets
RR_combined <- rbind(RR_stRW_dowry, RR_stIntI_RW_dowry)

# Plot both in the same ggplot
combined_plot <- ggplot(RR_combined, aes(x = year, y = RR, colour = model, fill = model)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.2, colour = NA) + 
  ggtitle("ST Models - Dowry Deaths") + 
  labs(x = "Year", y = "Relative Risk", colour = "Model", fill = "Model") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(RR_combined$year), max(RR_combined$year), by = 1))

print(combined_plot)

##########

# 16. Prepare for mapping spatial effects from interaction model
resRR_PP_stIntI_dowry <- data.frame(
  resRR = RR_stIntI_BYM_dowry,
  PP = unlist(PP_stIntI_BYM_dowry),
  ID_area = 1:70
)

# Create categories
resRR_PP_stIntI_dowry$resRRcat <- cut(resRR_PP_stIntI_dowry$resRR, 
                                      breaks = c(min(resRR_PP_stIntI_dowry$resRR), 
                                                 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6, 
                                                 max(resRR_PP_stIntI_dowry$resRR)),
                                      include.lowest = TRUE)

resRR_PP_stIntI_dowry$PPcat <- cut(resRR_PP_stIntI_dowry$PP, 
                                   breaks = c(0, 0.2, 0.8, 1.00), 
                                   include.lowest = TRUE)

# Join with spatial data
map_RR_ST_IntI_dowry <- merge(carto_up, resRR_PP_stIntI_dowry, by = "ID_area", all.x = TRUE)
map_RR_ST_IntI_dowry_sf <- st_as_sf(map_RR_ST_IntI_dowry)

# 17. Map spatial effects from interaction model
p5_dowry <- ggplot() + 
  geom_sf(data = map_RR_ST_IntI_dowry_sf, aes(fill = resRRcat)) +
  theme_bw() + 
  scale_fill_brewer(palette = "PuOr") + 
  guides(fill = guide_legend(title = "RR")) + 
  ggtitle("RR ST model Int I - Dowry Deaths") + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"))

p6_dowry <- ggplot() + 
  geom_sf(data = map_RR_ST_IntI_dowry_sf, aes(fill = PPcat)) +
  theme_bw() +
  scale_fill_viridis_d(
    option = "plasma", 
    name = "PP",
    direction = -1,
    guide = guide_legend(
      title.position = 'top',
      reverse = TRUE
    )) + 
  ggtitle("PP ST model Int I - Dowry Deaths") + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"))

# Compare all maps
all_maps_dowry <- (p1_dowry | p2_dowry) / (p3_dowry | p4_dowry) / (p5_dowry | p6_dowry)
print(all_maps_dowry)

# 18. Extract and visualize the space-time interaction terms
dowry_st_data$intI <- stIntI_BYM_dowry$summary.random$ID_space_time$mean

# Create categories for interaction values
dowry_st_data$intI_cat <- cut(
  dowry_st_data$intI,  
  breaks = c(-1, -0.05, -0.01, 0.01, 0.05, 1),
  include.lowest = TRUE
)

# Convert to sf for plotting
dowry_st_data_sf <- left_join(st_as_sf(carto_up), dowry_st_data, by = "ID_area")

# Plot interaction terms by year
interaction_plot <- ggplot() +
  geom_sf(data = dowry_st_data_sf, aes(fill = intI_cat)) + 
  theme_bw() +  
  scale_fill_brewer(palette = "PuOr") + 
  guides(fill = guide_legend(title = "Interaction")) + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Space-Time Interactions for Dowry Deaths (2001-2014)")

print(interaction_plot)

# 19. Extract hyperparameters for interpretation
hyper_table_dowry <- data.frame(
  median = stIntI_BYM_dowry$summary.hyperpar[, 4],
  LL = stIntI_BYM_dowry$summary.hyperpar[, 3],
  UL = stIntI_BYM_dowry$summary.hyperpar[, 5]
)
rownames(hyper_table_dowry) <- rownames(stIntI_BYM_dowry$summary.hyperpar)

# Format for display
hyper_table_dowry_rounded <- round(hyper_table_dowry, 3)

kable(hyper_table_dowry_rounded, 
      caption = "Posterior median and 95% CrI of hyperparameters for dowry deaths.") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")

# 20. Model comparison
model_comparison_dowry <- data.frame(
  Model = c("Spatial only", 
            "Space-time without interaction", 
            "Space-time with Type I interaction"),
  DIC = c(sBYM_dowry$dic$dic, 
          stBYM_dowry$dic$dic, 
          stIntI_BYM_dowry$dic$dic),
  WAIC = c(sBYM_dowry$waic$waic, 
           stBYM_dowry$waic$waic, 
           stIntI_BYM_dowry$waic$waic)
)

kable(model_comparison_dowry, 
      caption = "Model comparison for dowry deaths analysis.") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")

# 21. Calculate significant interactions
# Extract the interaction terms
interaction_terms <- stIntI_BYM_dowry$summary.random$ID_space_time$mean

# Calculate posterior probabilities for positive interactions
pp_int_pos <- numeric(length(interaction_terms))
for(i in 1:length(interaction_terms)) {
  marg <- stIntI_BYM_dowry$marginals.random$ID_space_time[[i]]
  pp_int_pos[i] <- 1 - inla.pmarginal(0, marg)
}

# Calculate posterior probabilities for negative interactions
pp_int_neg <- numeric(length(interaction_terms))
for(i in 1:length(interaction_terms)) {
  marg <- stIntI_BYM_dowry$marginals.random$ID_space_time[[i]]
  pp_int_neg[i] <- inla.pmarginal(0, marg)
}

# Identify significant interactions
sig_pos_interactions <- which(pp_int_pos > 0.95)
sig_neg_interactions <- which(pp_int_neg > 0.95)

cat("Number of significant positive interactions (PP > 0.95):", length(sig_pos_interactions), "\n")
cat("Number of significant negative interactions (PP > 0.95):", length(sig_neg_interactions), "\n")

# Create a data frame with districts, years, and interaction terms
interaction_df <- data.frame(
  District = dowry_st_data$dist[1:length(interaction_terms)],
  Year = dowry_st_data$year[1:length(interaction_terms)],
  Interaction = interaction_terms,
  PP_pos = pp_int_pos,
  PP_neg = pp_int_neg
)

# Identify the strongest positive interactions
top_positive <- interaction_df %>%
  filter(PP_pos > 0.95) %>%
  arrange(desc(Interaction)) %>%
  head(10)

# Identify the strongest negative interactions
top_negative <- interaction_df %>%
  filter(PP_neg > 0.95) %>%
  arrange(Interaction) %>%
  head(10)

# Print the results
cat("\nTop 10 strongest positive interactions:\n")
print(top_positive[, c("District", "Year", "Interaction", "PP_pos")])

cat("\nTop 10 strongest negative interactions:\n")
print(top_negative[, c("District", "Year", "Interaction", "PP_neg")])

# Count significant interactions by year
sig_by_year <- interaction_df %>%
  mutate(
    Significant_Pos = PP_pos > 0.95,
    Significant_Neg = PP_neg > 0.95
  ) %>%
  group_by(Year) %>%
  summarize(
    Positive_Count = sum(Significant_Pos),
    Negative_Count = sum(Significant_Neg),
    Total_Count = n()
  )

cat("\nSignificant interactions by year:\n")
print(sig_by_year)

##############################################
# Export all outputs to output_dir (change to your own folder of choice)

# Create the output directory if it doesn't exist
output_dir <- "C:/Users/benlh/OneDrive/Documents/00_HDAML/1_Bayesian/Mini project/ben_github/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Export all maps
# Spatial only model maps
ggsave(file.path(output_dir, "dowry_RR_spatial.png"), p1_dowry, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "dowry_PP_spatial.png"), p2_dowry, width = 8, height = 6, dpi = 300)

# Space-time model maps (no interaction)
ggsave(file.path(output_dir, "dowry_RR_st_no_int.png"), p3_dowry, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "dowry_PP_st_no_int.png"), p4_dowry, width = 8, height = 6, dpi = 300)

# Space-time model maps (with interaction)
ggsave(file.path(output_dir, "dowry_RR_st_int.png"), p5_dowry, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "dowry_PP_st_int.png"), p6_dowry, width = 8, height = 6, dpi = 300)

# Combined maps
ggsave(file.path(output_dir, "dowry_all_maps.png"), all_maps_dowry, width = 12, height = 15, dpi = 300)

# Temporal trends
ggsave(file.path(output_dir, "dowry_temp_no_int.png"), Temp1_dowry, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "dowry_temp_int.png"), Temp2_dowry, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_dir, "dowry_temp_comparison.png"), temp_comparison_dowry, width = 12, height = 6, dpi = 300)
ggsave(file.path(output_dir, "dowry_temp_combined.png"), combined_plot, width = 10, height = 6, dpi = 300)

# Interaction plots
ggsave(file.path(output_dir, "dowry_interaction_by_year.png"), interaction_plot, width = 12, height = 10, dpi = 300)

# Export data tables to CSV
# Model comparison
write.csv(model_comparison_dowry, file.path(output_dir, "dowry_model_comparison.csv"), row.names = FALSE)

# Hyperparameters
write.csv(hyper_table_dowry_rounded, file.path(output_dir, "dowry_hyperparameters.csv"))

# Significant interactions
write.csv(top_positive, file.path(output_dir, "dowry_top_positive_interactions.csv"), row.names = FALSE)
write.csv(top_negative, file.path(output_dir, "dowry_top_negative_interactions.csv"), row.names = FALSE)
write.csv(sig_by_year, file.path(output_dir, "dowry_significant_interactions_by_year.csv"), row.names = FALSE)

# Export full results data frames
write.csv(resRR_PP_dowry, file.path(output_dir, "dowry_spatial_results.csv"), row.names = FALSE)
write.csv(resRR_PP_st_dowry, file.path(output_dir, "dowry_st_no_int_results.csv"), row.names = FALSE)
write.csv(resRR_PP_stIntI_dowry, file.path(output_dir, "dowry_st_int_results.csv"), row.names = FALSE)
write.csv(RR_combined, file.path(output_dir, "dowry_temporal_trends.csv"), row.names = FALSE)
write.csv(interaction_df, file.path(output_dir, "dowry_all_interactions.csv"), row.names = FALSE)

# Save RDS files for later use
saveRDS(sBYM_dowry, file.path(output_dir, "dowry_spatial_model.rds"))
saveRDS(stBYM_dowry, file.path(output_dir, "dowry_st_no_int_model.rds"))
saveRDS(stIntI_BYM_dowry, file.path(output_dir, "dowry_st_int_model.rds"))

# Create HTML reports for key tables
# Hyperparameters table
hyper_html <- kable(hyper_table_dowry_rounded, 
                    caption = "Posterior median and 95% CrI of hyperparameters for dowry deaths.") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
save_kable(hyper_html, file.path(output_dir, "dowry_hyperparameters.html"))

# Model comparison table
model_html <- kable(model_comparison_dowry, 
                    caption = "Model comparison for dowry deaths analysis.") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE, position = "center")
save_kable(model_html, file.path(output_dir, "dowry_model_comparison.html"))

cat("All outputs successfully exported to", output_dir)