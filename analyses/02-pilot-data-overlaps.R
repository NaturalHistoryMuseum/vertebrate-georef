# Pilot analyses on amphibian data
# Getting the overlaps

# Load libraries
library(dplyr)
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rworldmap)
library(rgeos)

#---------------------------------------------
# Read in maps and check projection is latlong
# Name of this = maps
#----------------------------------------------
load("data/iucn-amphibian-maps.Rdata")

#--------------------------------------------------------
# Read in locality data and convert to spatial dataframe
#--------------------------------------------------------
ds <- read.csv("data/amphibian_data_example.csv")
str(ds)

# Remove NAs in lat long
ds <- ds[!is.na(ds$Latitude), ]

dsSPDF <- SpatialPointsDataFrame(ds[, c(13,12)], data.frame(ds[, 1:20]),
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
# Check species names match
# note that four new species are not in the IUCN maps so we remove them
setdiff(dsSPDF@data$ScientificName, maps$binomial)

dsSPDF@data <- filter(dsSPDF@data, ScientificName != "Hynobius hirosei"
                      & ScientificName != "Lissotriton meridionalis"
                        & ScientificName != "Ichthoyphis singaporensis"
                        & ScientificName != "Ichthyophis javanicus")

# Create array for results
overlaps <- array(NA, dim = c(length(unique(dsSPDF@data$ScientificName)), 4))
colnames(overlaps) <- c("Binomial", "NumberTypes", "NumberOverlaps", "PercentOverlaps")
overlaps <- data.frame(overlaps)

# Check this works as expected
overlaps

# Loop through for each species
for (i in 1:length(unique(dsSPDF@data$ScientificName))) {
  
  # Which species is species i?
  species_i <- as.character(unique(dsSPDF@data$ScientificName)[i])
  
  # How many Types do we have for that species?
  no_types <- length(which(dsSPDF@data$ScientificName == species_i))
  
  # Subset locality data so we just have locality for species i
  locality_i <- subset(dsSPDF, dsSPDF@data$ScientificName == species_i)
  
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

#--------------------------------------
# Summarise results
#--------------------------------------
# Summarise totals
summarise(overlaps,
          totaltypes = sum(NumberTypes),
          totaloverlaps = sum(NumberOverlaps),
          meanpercentoverlap = mean(PercentOverlaps, na.rm = TRUE))

# Merge overlaps with the taxonomic data
new_ds <- merge(ds[, 4:6], overlaps, by.x = "ScientificName", by.y = "Binomial")
new_ds <- unique(new_ds)

# Group by order
summarise(group_by(new_ds, Order),
          totaltypes = sum(NumberTypes),
          totaloverlaps = sum(NumberOverlaps),
          meanpercentoverlap = mean(PercentOverlaps))

# Finally you might want to save this dataset so you can use it in your analyses!
write_csv(path = "outputs/overlapsdata.csv", new_ds)

