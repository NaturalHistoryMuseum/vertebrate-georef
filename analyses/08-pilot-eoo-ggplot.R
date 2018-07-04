# Extracting convex hulls from GBIF data
# Natalie Cooper
# July 2018
#---------------------------------------------------------------
# load libraries
library(tidyverse)
library(sf)
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rworldmap)
library(rgeos)
library(spatstat)
library(ggmap)
library(viridis)

# Convex hulls function...
make.hull <- function(data){
  hull <- spatstat::convexhull.xy(data$decimallongitude, data$decimallatitude)
  data.frame(x = hull$bdry[[1]]$x, y = hull$bdry[[1]]$y)
}

# Convert SpatialPolygonsDataFrame to dataframe for plotting
convert.spdf <- function(map) {
  map@data$id <- rownames(map@data)
  map.points <- fortify(map, region = "id")
  map.df <- dplyr::full_join(map.points, map@data, by = "id")
  return(map.df)
}

#---------------------------------------------------------------
# Read in the gbif data
gbif <- read_csv("data/salamander-gbif.csv")
glimpse(gbif)
# Filter out records with no localities
gbif <- gbif %>%
  filter(!is.na(decimallatitude)) %>%
  filter(year < 1901)

# Extract just USA map
usamap <- map_data("usa")

# Read in salamander IUCN maps (takes a while)
maps <- readOGR("data/CAUDATA")

# Subset maps for the three species
maps_noto <- subset(maps, maps@data$binomial == "Notophthalmus viridescens")
maps_tg <- subset(maps, maps@data$binomial == "Taricha granulosa")

# Convert these so they can be plotted in ggplot
maps_noto.df <- convert.spdf(maps_noto)
maps_tg.df <- convert.spdf(maps_tg)

# Quick simplification of map polygons
maps_noto.df <- maps_noto.df %>%
filter(piece == 1)

maps_tg.df <- maps_tg.df %>%
filter(piece == 1)

# Make individual species datasets for hulls
gbif_noto <- gbif %>%
  filter(species == "Notophthalmus viridescens") %>%
  mutate(species = str_replace(species, "Notophthalmus ", "N."))

gbif_tg <- gbif %>%
  filter(species == "Taricha granulosa") %>%
  mutate(species = str_replace(species, "Taricha ", "T."))

gbif_two <- rbind(gbif_noto, gbif_tg)

# Make hulls for each species
hull_noto <- make.hull(gbif_noto)
hull_tg <- make.hull(gbif_tg)

#---------------------------------------------------------------
# Plot maps
ggplot(data = usamap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, col = "black", size = 0.5) +
  geom_polygon(data = hull_noto, aes(x = x, y = y, group = NULL), alpha = 0.2) +
  geom_polygon(data = hull_tg, aes(x = x, y = y, group = NULL), alpha = 0.2) +
  geom_point(data = gbif_two, aes(x = decimallongitude, y = decimallatitude, 
                                   group = NA, shape = species, color = species), 
                                   alpha = 0.9) +
  geom_polygon(data = maps_noto.df, fill = NA, col = "black", linetype = "dotted") +
  geom_polygon(data = maps_tg.df, fill = NA, col = "black", linetype = "dotted") +
  scale_colour_manual(values = c(viridis(4)[1:3])) +
  theme_void() +
  theme(legend.position = c(0.3, 0.7),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic")) + 
 guides(colour = guide_legend(override.aes = list(size = 2),
                              keyheight = 0.8, keywidth = 0.5))

ggsave("outputs/convex-hulls-maps.png", height = 5, width = 8, 
       units = c("cm"))
