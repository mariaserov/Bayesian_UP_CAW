# Script to create a map of Uttar Pradesh with all district names labeled

library(sf)          # For spatial data handling
library(ggplot2)     # For plotting
library(dplyr)       # For data manipulation
library(ggrepel)     # For non-overlapping text labels

# load("~/OneDrive/Documents/00_HDAML/1_Bayesian/Mini project/DS6_CrimeUttarPradesh/CrimeUttarPradesh.RData")

# Convert the SpatialPolygonsDataFrame to an sf object
carto_up_sf <- st_as_sf(carto_up)

# Calculate centroids for each district to place labels
carto_up_sf$centroid <- st_centroid(st_geometry(carto_up_sf))

# Extract coordinates for labeling
centroids <- data.frame(
  district = carto_up_sf$dist,
  ID_area = carto_up_sf$ID_area,
  st_coordinates(carto_up_sf$centroid)
)
names(centroids)[3:4] <- c("x", "y")

# Create a plain map of Uttar Pradesh with district names
district_map <- ggplot() +
  geom_sf(data = carto_up_sf, fill = "white", color = "darkgrey") +
  geom_text_repel(
    data = centroids,
    aes(x = x, y = y, label = district),
    size = 2.5,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.1,
    min.segment.length = 0.1,
    max.overlaps = 30  # Adjust as needed
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Districts of Uttar Pradesh",
    subtitle = "Reference map with district names",
    caption = "Data source: National Crime Records Bureau"
  )

# Display the map
print(district_map)

# Create a map with district ID numbers (sometimes easier to reference)
id_map <- ggplot() +
  geom_sf(data = carto_up_sf, fill = "white", color = "darkgrey") +
  geom_text_repel(
    data = centroids,
    aes(x = x, y = y, label = ID_area),
    size = 3,
    fontface = "bold",
    box.padding = 0.5,
    point.padding = 0.1,
    min.segment.length = 0.1,
    max.overlaps = 100
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Districts of Uttar Pradesh",
    subtitle = "Reference map with district ID numbers",
    caption = "Data source: National Crime Records Bureau"
  )

# Display the ID map
print(id_map)

# Create a combined reference map with both names and IDs
combined_map <- ggplot() +
  geom_sf(data = carto_up_sf, fill = "white", color = "darkgrey") +
  geom_text_repel(
    data = centroids,
    aes(x = x, y = y, label = paste0(district, " (", ID_area, ")")),
    size = 2,
    fontface = "bold",
    box.padding = 0.35,
    point.padding = 0.1,
    min.segment.length = 0.1,
    max.overlaps = 15
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Districts of Uttar Pradesh",
    subtitle = "Reference map with district names and ID numbers",
    caption = "Data source: National Crime Records Bureau"
  )

# Display the combined map
print(combined_map)

# Save all maps as PDF files for better quality printing
ggsave("uttar_pradesh_district_names.pdf", district_map, width = 11, height = 8)
ggsave("uttar_pradesh_district_ids.pdf", id_map, width = 11, height = 8)
ggsave("uttar_pradesh_names_idnum.pdf", combined_map, width = 11, height = 8)

# Create a lookup table matching district names to ID numbers
lookup_table <- data.frame(
  ID_area = carto_up_sf$ID_area,
  District = carto_up_sf$dist
) %>%
  arrange(ID_area)

# Display the lookup table
print(lookup_table)

# Write lookup table to CSV for easy reference
write.csv(lookup_table, "uttar_pradesh_district_lookup.csv", row.names = FALSE)