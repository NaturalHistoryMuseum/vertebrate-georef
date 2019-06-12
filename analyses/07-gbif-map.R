# Plot the GBIF records
# to make Figure 3 for proposal

# Load libraries
library(tidyverse)
library(viridis)

# Read in all gbif records
gbif <- read_csv("data/all-gbif.csv")
glimpse(gbif)

# Remove post 1900 records and those with no year
gbif <- 
  gbif %>%
  filter(year < 1901 & !is.na(year))
glimpse(gbif)

# Add clade column, remove sea squirts and make a "fishes" "clade"
gbif <- 
  gbif %>%
  filter(class != "Ascidiacea" & class != "Thaliacea" & !is.na(class)) %>%
  mutate(clade = class) %>%
  mutate(clade = if_else(clade == "Amphibia", "Amphibians", clade)) %>%
  mutate(clade = if_else(clade == "Reptilia", "Reptiles", clade)) %>%
  mutate(clade = if_else(clade == "Aves", "Birds", clade)) %>%
  mutate(clade = if_else(clade == "Mammalia", "Mammals", clade)) %>%
  mutate(clade = if_else(clade == "Actinopterygii", "Fishes", clade)) %>%
  mutate(clade = if_else(clade == "Cephalaspidomorphi", "Fishes", clade)) %>%
  mutate(clade = if_else(clade == "Elasmobranchii", "Fishes", clade)) %>%
  mutate(clade = if_else(clade == "Leptocardii", "Fishes", clade))
glimpse(gbif)

# Extract map and plot
worldmap <- map_data("world")
ggplot(data = worldmap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "#dddddd", col = "#dddddd", size = 0.5) +
  # Add points with colour as clade
  geom_point(data = gbif, aes(x = decimallongitude, y = decimallatitude, 
                              group = NA, colour = clade), size = 0.4, alpha = 1, pch = 16) +
  scale_colour_manual(values = c(viridis(5))) +
  theme_void() +
  # Legend manipulations
  theme(legend.position = "top",
        legend.title = element_blank()) +
  guides(colour = guide_legend(keyheight = 0.8, keywidth = 0.5,
                               override.aes = list(size = 2)))

# Save figure
ggsave("figures/all-gbif-map.png", height = 5, width = 8, 
       units = c("cm"))
