# Pilot analyses on amphibian data
# Getting the overlaps of GBIF salamanders
# to add numbers to text of proposal

# Load libraries
library(dplyr)
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rworldmap)
library(rgeos)

#---------------------------------------------
# Read in maps 
#----------------------------------------------
maps <- readOGR("data/CAUDATA")

#--------------------------------------------------------
# Read in locality data and convert to spatial dataframe
#--------------------------------------------------------
gbif <- read_csv("data/salamander-gbif.csv")
glimpse(gbif)
# Filter out records with no localities
gbif <- gbif %>%
  filter(!is.na(decimallatitude)) %>%
  filter(year < 1901)

ds <- gbif %>%
  filter(species == "Taricha granulosa" | species == "Notophthalmus viridescens")

# Remove NAs in lat long
ds <- ds[!is.na(ds$decimallatitude), ]

dsSPDF <- SpatialPointsDataFrame(ds[, c(18,17)], data.frame(ds[, 1:44]),
                                 proj4string = CRS("+proj=longlat +datum=WGS84 
                                                   +no_defs +ellps=WGS84 
                                                   +towgs84=0,0,0"))
# creates dataframe - first thing needs to be long then lat, then the second is all the data
# order is CRUCIAL: LONGITUDE then LATITUDE
# CRS makes sure the projection is correct

#-----------------------------------------------
# Loop to extract overlaps
# Will write functions for this at some stage
#-----------------------------------------------

# Create array for results
overlaps <- array(NA, dim = c(length(unique(dsSPDF@data$species)), 4))
colnames(overlaps) <- c("Binomial", "NumberTypes", "NumberOverlaps", "PercentOverlaps")
overlaps <- data.frame(overlaps)

# Check this works as expected
overlaps

# Loop through for each species
for (i in 1:length(unique(dsSPDF@data$species))) {
  
  # Which species is species i?
  species_i <- as.character(unique(dsSPDF@data$species)[i])
  
  # How many Types do we have for that species?
  no_types <- length(which(dsSPDF@data$species == species_i))
  
  # Subset locality data so we just have locality for species i
  locality_i <- subset(dsSPDF, dsSPDF@data$species == species_i)
  
  # Subset map data so we just have map for species i
  map_i <- subset(maps, maps$binomial == species_i)
  
  # How many of the Type localities overlap with range polygons?
  no_overlaps <- sum(!is.na(over(locality_i, as(map_i, "SpatialPolygons"))))
  
  # Outputs
  # I have added the number and % of overlaps as some
  # species have > 1 type
  overlaps[i, "Binomial"] <- species_i
  overlaps[i, "NumberTypes"] <- no_types
  overlaps[i, "NumberOverlaps"] <- no_overlaps
  overlaps[i, "PercentOverlaps"] <- (no_overlaps/no_types) * 100
  
} # end loop

# Look at the output
overlaps
