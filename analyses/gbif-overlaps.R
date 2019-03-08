#-----------------------------
# GBIF data - getting overlaps
# Natalie Cooper
# March 2019
#-----------------------------

# Load libraries
library(tidyverse)
library(viridis)
library(knitr)
library(sf)
library(maptools)
library(rgdal)
library(sp)
library(raster)
library(rworldmap)
library(rgeos)
library(spatstat)
library(ggmap)
library(here)
library(ggspatial)

# Short function to get decades
floor_decade <- function(x){
  if(!is.na(x)){
    x - x %% 10
  }else{NA_character_}
}

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
```

```{r, echo = FALSE, message = FALSE, warning = FALSE, error = FALSE}
# Read in the tidied GBIF data
gbif <- read_csv(here("data/tidy-gbif.csv"))
```

```{r}
# Read in maps
# This takes a long time and a lot of memory so I recommend doing one clade at a time
#maps_amphibian <- readOGR(here("data/AMPHIBIANS"))
#maps_mammal <- readOGR(here("data/MAMMALS"))
#maps_bird <- readOGR("data/BIRDS")
```

```{r, echo = FALSE}
# First find only those species with >= 3 specimens with unique localities
specimen3 <- 
  gbif %>%
  distinct(clade, binomial, decimallongitude, decimallatitude) %>%
  group_by(clade, binomial) %>%
  summarise(n = n()) %>%
  filter(n >= 3)

# Then select these specimens from the gbif data
gbif3 <- gbif2[gbif2$binomial %in% specimen3$binomial, ]
```


```{r, echo = FALSE}
# Split by clade
gbif3_amphibian <- 
  gbif3 %>%
  filter(clade == "Amphibians")

gbif3_bird <- 
  gbif3 %>%
  filter(clade == "Birds")

gbif3_mammal <- 
  gbif3 %>%
  filter(clade == "Mammals")

```


```{r}
# Create convex hulls for all species with >= 3 unique localities
hull_species_amphibian <- list()
hull_species_bird <- list()
hull_species_mammal <- list()

# Make hulls                              
for(i in 1:length(unique(gbif3_amphibian$binomial))){
  hull_species_amphibian[[i]] <- make.hull(gbif3_amphibian[gbif3_amphibian$binomial ==         
                                                          unique(gbif3_amphibian$binomial)[i], ])
}

for(i in 1:length(unique(gbif3_bird$binomial))){
  hull_species_bird[[i]] <- make.hull(gbif3_bird[gbif3_bird$binomial ==         
                                                unique(gbif3_bird$binomial)[i], ])
}

for(i in 1:length(unique(gbif3_mammal$binomial))){
  hull_species_mammal[[i]] <- make.hull(gbif3_mammal[gbif3_mammal$binomial ==         
                                                        unique(gbif3_mammal$binomial)[i], ])
}

# Add species names
names(hull_species_amphibian) <- unique(gbif3_amphibian$binomial)
names(hull_species_bird) <- unique(gbif3_bird$binomial)
names(hull_species_mammal) <- unique(gbif3_mammal$binomial)
```

```{r, echo = FALSE}
# Convert to spatial points dataframe
dsSPDF_amphibian <- SpatialPointsDataFrame(gbif3_amphibian[, c(10,11)], 
                                           data.frame(gbif3_amphibian[, 1:11]),
                                           proj4string = CRS("+proj=longlat +datum=WGS84 
                                                              +no_defs +ellps=WGS84 
                                                              +towgs84=0,0,0"))

dsSPDF_bird <- SpatialPointsDataFrame(gbif3_bird[, c(10,11)], 
                                           data.frame(gbif3_bird[, 1:11]),
                                           proj4string = CRS("+proj=longlat +datum=WGS84 
                                                              +no_defs +ellps=WGS84 
                                                              +towgs84=0,0,0"))

dsSPDF_mammal <- SpatialPointsDataFrame(gbif3_mammal[, c(10,11)], 
                                           data.frame(gbif3_mammal[, 1:11]),
                                           proj4string = CRS("+proj=longlat +datum=WGS84 
                                                              +no_defs +ellps=WGS84 
                                                              +towgs84=0,0,0"))

```

```{r}
# Loop to extract overlaps
# Will write functions for this at some stage

# Create arrays for results
overlaps_amphibian <- array(NA, dim = c(length(unique(dsSPDF_amphibian@data$binomial)), 4))
colnames(overlaps_amphibian) <- c("Binomial", "NumberSpecimens", "NumberOverlaps", "PercentOverlaps")
overlaps_amphibian <- data.frame(overlaps_amphibian)

overlaps_bird <- array(NA, dim = c(length(unique(dsSPDF_bird@data$binomial)), 4))
colnames(overlaps_bird) <- c("Binomial", "NumberSpecimens", "NumberOverlaps", "PercentOverlaps")
overlaps_bird <- data.frame(overlaps_bird)

overlaps_mammal <- array(NA, dim = c(length(unique(dsSPDF_mammal@data$binomial)), 4))
colnames(overlaps_mammal) <- c("Binomial", "NumberSpecimens", "NumberOverlaps", "PercentOverlaps")
overlaps_mammal <- data.frame(overlaps_mammal)

# Loop through for each species
for (i in 1:length(unique(dsSPDF_amphibian@data$binomial))) {
  
  # Which species is species i?
  species_i <- as.character(unique(dsSPDF_amphibian@data$binomial)[i])
  # How many Specimens do we have for that species?
  no_types <- length(which(dsSPDF_amphibian@data$binomial == species_i))
  # Subset locality data so we just have locality for species i
  locality_i <- subset(dsSPDF_amphibian, dsSPDF_amphibian@data$binomial == species_i)
  # Subset map data so we just have map for species i
  map_i <- subset(maps_amphibian, maps_amphibian$binomial == species_i)
  # How many of the Type localities overlap with range polygons?
  no_overlaps <- sum(!is.na(over(locality_i, as(map_i, "SpatialPolygons"))))
  
  # Outputs
  # I have added the number and % of overlaps as some
  # species have > 1 type
  overlaps_amphibian[i, "Binomial"] <- species_i
  overlaps_amphibian[i, "NumberSpecimens"] <- no_types
  overlaps_amphibian[i, "NumberOverlaps"] <- no_overlaps
  overlaps_amphibian[i, "PercentOverlaps"] <- (no_overlaps/no_types) * 100
  
} # end loop
```

```{r, eval = FALSE}
# Birds
# Loop through for each species
for (i in 1:length(unique(dsSPDF_bird@data$binomial))) {
  
  # Which species is species i?
  species_i <- as.character(unique(dsSPDF_bird@data$binomial)[i])
  # How many Specimens do we have for that species?
  no_types <- length(which(dsSPDF_bird@data$binomial == species_i))
  # Subset locality data so we just have locality for species i
  locality_i <- subset(dsSPDF_bird, dsSPDF_bird@data$binomial == species_i)
  # Subset map data so we just have map for species i
  map_i <- subset(maps_bird, maps_bird$binomial == species_i)
  # How many of the Type localities overlap with range polygons?
  no_overlaps <- sum(!is.na(over(locality_i, as(map_i, "SpatialPolygons"))))
  
  # Outputs
  # I have added the number and % of overlaps as some
  # species have > 1 type
  overlaps_bird[i, "Binomial"] <- species_i
  overlaps_bird[i, "NumberSpecimens"] <- no_types
  overlaps_bird[i, "NumberOverlaps"] <- no_overlaps
  overlaps_bird[i, "PercentOverlaps"] <- (no_overlaps/no_types) * 100
  
} # end loop

```

```{r}

#maps_mammal <- readOGR(here("data/MAMMALS"))

# Loop through for each species
for (i in 1:length(unique(dsSPDF_mammal@data$binomial))) {
  
  # Which species is species i?
  species_i <- as.character(unique(dsSPDF_mammal@data$binomial)[i])
  # How many Specimens do we have for that species?
  no_types <- length(which(dsSPDF_mammal@data$binomial == species_i))
  # Subset locality data so we just have locality for species i
  locality_i <- subset(dsSPDF_mammal, dsSPDF_mammal@data$binomial == species_i)
  # Subset map data so we just have map for species i
  map_i <- subset(maps_mammal, maps_mammal$binomial == species_i)
  # How many of the Type localities overlap with range polygons?
  no_overlaps <- sum(!is.na(over(locality_i, as(map_i, "SpatialPolygons"))))
  
  # Outputs
  # I have added the number and % of overlaps as some
  # species have > 1 type
  overlaps_mammal[i, "Binomial"] <- species_i
  overlaps_mammal[i, "NumberSpecimens"] <- no_types
  overlaps_mammal[i, "NumberOverlaps"] <- no_overlaps
  overlaps_mammal[i, "PercentOverlaps"] <- (no_overlaps/no_types) * 100
  
} # end loop

write_csv(overlaps_mammal, path = here("data/overlaps_mammal.csv"))

```

```{r}
# Look at the output
overlaps_amphibian
#overlaps_bird
#overlaps_mammal

# Save output
write_csv(overlaps_amphibian, path = here("data/overlaps_amphibian.csv"))
#write_csv(overlaps_bird, path = here("data/overlaps_bird.csv"))
#write_csv(overlaps_mammal, path = here("data/overlaps_mammal.csv"))
```


```{r}
# Loop to extract area of overlap
# Will write functions for this at some stage

# Create arrays for results
areas_amphibian <- array(NA, dim = c(length(unique(dsSPDF_amphibian@data$binomial)), 4))
colnames(areas_amphibian) <- c("Binomial", "AreaHull", "AreaOverlaps", "PercentOverlaps")
areas_amphibian <- data.frame(areas_amphibian)

areas_bird <- array(NA, dim = c(length(unique(dsSPDF_bird@data$binomial)), 4))
colnames(areas_bird) <- c("Binomial", "AreaHull", "AreaOverlaps", "PercentOverlaps")
areas_bird <- data.frame(areas_bird)

areas_mammal <- array(NA, dim = c(length(unique(dsSPDF_mammal@data$binomial)), 4))
colnames(areas_mammal) <- c("Binomial", "AreaHull", "AreaOverlaps", "PercentOverlaps")
areas_mammal <- data.frame(areas_mammal)

# Loop through for each species
for (i in z) { # need to skip the ones with no map
  
  # Which species is species i?
  species_i <- as.character(unique(dsSPDF_mammal@data$binomial)[i])
  # Select hull for species_i
  hull_i <- hull_species_mammal[[species_i]]
  # Make hull a spatial polygon object
  hull_poly_i <- SpatialPolygons(list(Polygons(list(Polygon(hull_i)), ID=1)))
  
  # Subset map data so we just have map for species i
  map_i <- subset(maps_mammal, maps_mammal$binomial == species_i)
  # Get intersection of map and hull
  poly_overlap_i <- intersect(map_i, hull_poly_i)
  
  # Get areas
  hull_area <- sum(areaPolygon(hull_poly_i))
  map_area <- sum(areaPolygon(map_i))
  overlap_area <- sum(areaPolygon(poly_overlap_i))
  
  # Outputs
  # I have added the number and % of areas as some
  # species have > 1 type
  areas_mammal[i, "Binomial"] <- species_i
  areas_mammal[i, "AreaHull"] <- hull_area
  areas_mammal[i, "AreaOverlaps"] <- overlap_area
  areas_mammal[i, "PercentOverlaps"] <- (overlap_area/hull_area) * 100
  
} # end loop

# Look at the output
#areas_amphibian
#areas_bird
areas_mammal

#write_csv(areas_amphibian, path = here("data/areas_amphibian.csv"))
#write_csv(areas_bird, path = here("data/areas_bird.csv"))
write_csv(areas_mammal, path = here("data/areas_mammal.csv"))
```

```{r}
# To check intersect works, pick one species and plot map, hull and overlap
i <- 1
species_i <- as.character(unique(dsSPDF_amphibian@data$binomial)[i])
hull_i <- hull_species_amphibian[[species_i]]
hull_poly_i <- SpatialPolygons(list(Polygons(list(Polygon(hull_i)), ID=1)))
map_i <- subset(maps_amphibian, maps_amphibian$binomial == species_i)
poly_overlap_i <- intersect(map_i, hull_poly_i)

# Convert polygons so we can plot them in ggplot
map_i2<- convert.spdf(map_i)
maps_i2 <- map_i2 %>%
  filter(piece == 1)

poly_overlap_i2<- convert.spdf(poly_overlap_i)
poly_overlap_i2 <- poly_overlap_i2 %>%
  filter(piece == 1)

world <- map_data("usa")

ggplot(data = world, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, col = "black", size = 0.5) +
  geom_polygon(data = map_i2, aes(x = long, y = lat, group = NULL), fill = "black") +
  geom_polygon(data = hull_i, aes(x = x, y = y, group = NULL), fill = "red") +
  geom_polygon(data = poly_overlap_i2, aes(x = long, y = lat, group = NULL), fill = "blue") +
  geom_point(data = gbif3_amphibian[gbif3_amphibian$binomial == species_i, ],
             aes(x = decimallongitude, y = decimallatitude, group = NULL))
```

