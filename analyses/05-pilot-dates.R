# load libraries
library(tidyverse)

# Read in the gbif data
gbif <- read_csv("data/salamander-gbif.csv")
glimpse(gbif)
# Filter out records with no localities
gbif <- gbif %>%
  filter(!is.na(decimallatitude)) %>%
  filter(year < 1901)
  
# Read in the nhm data
amphibians <- read_csv("data/amphibian_data_example.csv")
# Select only salamanders
sal <- filter(amphibians, Order == "Caudata")
# Extract dates from registration numbers
sal <-
  sal %>%
  mutate(year = as.numeric(str_extract(sal$RegistrationNumber, "\\d{4}"))) %>%
  filter(year < 1901)

# Match up species in both 
intersect(sal$ScientificName, gbif$species)
# No species match up

median(gbif$year)
median(sal$year)

