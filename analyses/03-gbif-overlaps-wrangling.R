# Overlaps data wrangling to add region data etc.

# Load libraries
library(tidyverse)
library(here)

#----------------------------------
# Add taxonomic data
#----------------------------------
# Read in the tidied GBIF data
gbif <- read_csv(here("data/tidy-gbif.csv"))
# Extract just taxonomic info
gbif2 <- gbif %>%
  dplyr::select(clade, order, family, binomial) %>%
  distinct()

# Read in overlaps and join all together
amph <- read_csv(here("data/overlaps_amphibian.csv"))
bird <- read_csv(here("data/overlaps_bird.csv"))
mamm <- read_csv(here("data/overlaps_mammal.csv"))
overlaps <- rbind(amph, bird, mamm)

# Merge the taxonomy with the overlaps and remove NA rows
ds <- overlaps %>%
  left_join(gbif2, by = c("Binomial" = "binomial")) %>%
  drop_na()

# Remove large files
rm(gbif)
rm(gbif2)

#----------------------------------
# Add region/continent data
#----------------------------------
# Read in full gbif dataset
gbif <- read_csv(here("data/all-gbif.csv"))

# Wrangle to just taxonomy and country codes
gbif <- gbif %>%
  # Get taxonomy as required 
  separate(species, c("Genus", "Species"), sep = " ", 
           extra = "drop", fill = "right") %>%
    filter(!is.na(Species) & Species != "sp." & Species != "sp") %>%
    unite("binomial", Genus, Species, sep = " ") %>%

  # Select just the columns of interest
  dplyr::select(binomial, countrycode) %>%
  
  # Remove duplicates
  distinct() %>%
  
  # Remove NAs
  drop_na()

# Join with overlaps data
q <- full_join(ds, gbif, by = c("Binomial" = "binomial"))

# Read in country code data and join
# Remove column with no data (X5)
# Remove the reptile record as we don't have the maps
# Remove the ones with no continent listed (all birds, not sure why)
codes <- read_csv(here("data/country-codes.csv"))
qq <- q %>%
  left_join(codes, by = c("countrycode" = "code")) %>%
  dplyr::select(-X5) %>%
  filter(!is.na(clade) & clade != "Reptiles" & !is.na(continent))

#----------------------------------
# Write to file
#----------------------------------
write_csv(qq, path = here("data/gbif-overlaps.csv"))

### Remember to delete duplicates if using region/continent not country ###

qq %>%
  dplyr::select(-country, -region, -countrycode) %>%
  distinct() %>%
  group_by(clade,continent) %>%
  summarise(n())






