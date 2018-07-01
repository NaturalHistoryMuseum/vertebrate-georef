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

# Read in map and extract just USA/CANADA
data(wrld_simpl)
usa <- subset(wrld_simpl, wrld_simpl$ISO3 == "USA" | wrld_simpl$ISO3 == "CAN")

# Read in amphibian maps
# Subset out three species
maps <- readOGR("data/CAUDATA")
maps_noto <- subset(maps, maps@data$binomial == "Notophthalmus viridescens")
maps_tg <- subset(maps, maps@data$binomial == "Taricha granulosa")
maps_tt <- subset(maps, maps@data$binomial == "Taricha torosa")

png("outputs/range-maps.png", height = 1000, width = 1000)
plot(usa)
plot(maps_noto, add = TRUE, col = rgb(0, 0, 1, 0.5))
plot(maps_tg, add = TRUE, col = rgb(0, 0, 1, 0.5))
plot(maps_tt, add = TRUE, col = rgb(0, 0, 1, 0.5))
dev.off()
