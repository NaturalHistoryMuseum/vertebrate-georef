# Pilot analyses on amphibian data

# Load libraries
library(tidyverse)
library(ggmap)
library(viridisLite)

# Read in the data
# This is not yet properly validated and needs some tidying
amphibians <- read_csv("data/amphibian_data_example.csv")
glimpse(amphibians)

# Extract world map data
worldmap <- map_data("world")

# Plot map (as polygons) then add locality points
ggplot(data = worldmap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, col = "black", size = 0.1) +
  geom_point(data = amphibians, aes(x = Longitude, y = Latitude, 
                                    group = NA, color = Order, shape = Order), 
             size = 1, alpha = 1) +
  theme_void() +
  theme(legend.position = c(0.15, 0.3),
        legend.title=element_blank()) + 
  scale_colour_manual(values =c(viridis(4)[1:3])) +
  guides(colour = guide_legend(override.aes = list(size=1),
                               keyheight = 0.8, keywidth = 0.5))

ggsave("outputs/GeoRefSites.png", height = 5, width = 8, 
       units = c("cm"))

amphibians %>%
  group_by(Order) %>%
  summarise(n())
