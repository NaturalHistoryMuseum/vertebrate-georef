# Plot the GBIF records
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
  geom_polygon(fill = NA, col = "black", size = 0.5) +
  geom_point(data = gbif, aes(x = decimallongitude, y = decimallatitude, 
                              group = NA, colour = clade), size = 0.25, alpha = 0.8, pch = 16) +
  scale_colour_manual(values = c(viridis(5))) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  guides(colour = guide_legend(keyheight = 0.8, keywidth = 0.5,
                               override.aes = list(size = 2)))

ggsave("outputs/all-gbif-map.png", height = 5, width = 8, 
       units = c("cm"))
