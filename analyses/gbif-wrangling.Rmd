# Investigate GBIF records
# Natalie Cooper
# March 2019
#---------------------------------
# Load libraries
library(tidyverse)
library(viridis)

# Short function to get decades
floor_decade <- function(x){
  if(!is.na(x)){
    x - x %% 10
  }else{NA_character_}
}

#---------------------------------------------------------------
# Read in the GBIF data
#---------------------------------------------------------------
gbif <- read_csv("data/all-gbif.csv")
glimpse(gbif)

#---------------------------------------------------------------
# Wrangle the data so it is useable
#---------------------------------------------------------------

gbif2 <- 
  gbif %>%
  
  # Check that all included records are specimens not observations
  filter(basisofrecord == "PRESERVED_SPECIMEN") %>%
  
  # Create a new column for specimen ID number
  unite(col = specID, `institutioncode`, `catalognumber`, 
        sep = "_", remove = FALSE) %>%
  
  # Remove post 1900 records and those with no year
  filter(year < 1901 & !is.na(year))  %>%

  # Add decade variable (function above)
  # This maps to character to deal with NAs so needs coercing back to numeric
  mutate(decade = map_chr(year, floor_decade)) %>%
  mutate(decade = as.numeric(decade)) %>%

  # Add clade column, remove sea squirts and make a "fishes" "clade"
  filter(class != "Ascidiacea" & class != "Thaliacea" & !is.na(class)) %>%
  mutate(clade = class) %>%
  mutate(clade = if_else(clade == "Amphibia", "Amphibians", clade)) %>%
  mutate(clade = if_else(clade == "Reptilia", "Reptiles", clade)) %>%
  mutate(clade = if_else(clade == "Aves", "Birds", clade)) %>%
  mutate(clade = if_else(clade == "Mammalia", "Mammals", clade)) %>%
  mutate(clade = if_else(clade == "Actinopterygii", "Fishes", clade)) %>%
  mutate(clade = if_else(clade == "Cephalaspidomorphi", "Fishes", clade)) %>%
  mutate(clade = if_else(clade == "Elasmobranchii", "Fishes", clade)) %>%
  mutate(clade = if_else(clade == "Leptocardii", "Fishes", clade)) %>%

  # Remove records with no lat/long
  filter(!is.na(decimallatitude) & !is.na(decimallongitude)) %>%
  
  # TAXONOMY (note there is a second set of checks later)
  # 1. Remove entries where we only have the Genus
  # 2. Create binomial column
  separate(species, c("Genus", "Species"), sep = " ", 
           extra = "drop", fill = "right") %>%
    filter(!is.na(Species) & Species != "sp." & Species != "sp") %>%
    unite("binomial", Genus, Species, sep = " ") %>%

  # Select just the columns of interest
  dplyr::select(specID, binomial, clade, class, order, family, genus,
           year, decade, decimallongitude, decimallatitude) %>%
  
  # Remove duplicates
  distinct()

#---------------------------------------------------------------
# Summary data
#---------------------------------------------------------------

sum_decade <- gbif2 %>%
     group_by(clade, decade, binomial) %>%
     summarise(n = n())

sum_species <- gbif2 %>%
  group_by(clade, binomial) %>%
  summarise(n = n())

ggplot(sum_species, aes(x = n)) +
  geom_histogram(bins = 10) +
  xlim(0, 25) +
  facet_wrap(~ clade, scales = "free") +
  theme_bw()

boo <-
  gbif2 %>%
  group_by(clade, binomial, decimallongitude, decimallatitude) %>%
  summarise(n = n()) %>%
  filter(n >= 10)

world <- map_data("world")

ggplot(data = world, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = NA, col = "black", size = 0.5) +
  #geom_polygon(data = hull_noto, aes(x = x, y = y, group = NULL), alpha = 0.2) +
  #geom_polygon(data = hull_tg, aes(x = x, y = y, group = NULL), alpha = 0.2) +
  geom_point(data = boo, aes(x = decimallongitude, y = decimallatitude, 
                                  group = NA, color = binomial), alpha = 0.9) +
  facet_wrap( ~ clade) +
  theme(legend.position = "none")
