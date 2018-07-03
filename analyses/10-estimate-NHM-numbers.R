# Estimating numbers from NHM 

library(tidyverse)
library(readxl)

# Read in data
herp <- read_xlsx("data/NERC_georef/Amphibians_and_reptiles/May_2016_BMNH_herp_data.xlsx")
bird <- read_csv("data/NERC_georef/Birds/occurrence.csv")
mamm <- read_csv("data/NERC_georef/Mammals/occurrence.csv")
fish <- read_csv("data/NERC_georef/Fishes/occurrence.csv")
  
# Summarise the data
herp %>%
  # Extract dates from registration numbers
  mutate(year = as.numeric(str_extract(herp$RegRegistrationNumber, "\\d{4}"))) %>%
  # Remove pre 1901 specimens
  filter(year < 1901 & !is.na(year) & year > 1750) %>%
  # Exclude without species names
  filter(!is.na(DarScientificName)) %>%
  # Exclude without locality data
  filter(!is.na(DarLocality)) %>%
  # Split into class and get total number of specimens
  group_by(DarClass) %>%
  summarise(n(),
            length(unique(DarScientificName)))
# NA class = Reptiles

bird %>%
  # Extract dates from registration numbers
  mutate(year = as.numeric(str_extract(bird$catalogNumber, "\\d{4}"))) %>%
  # Extract only zoology records
  filter(collectionCode == "ZOO") %>%
  # Remove pre 1901 specimens
  filter(year < 1901 & !is.na(year) & year > 1750) %>%
  # Exclude without species names
  filter(!is.na(scientificName)) %>%
  # Exclude without locality data
  filter(!is.na(locality)) %>%
  # Get total number of specimens and species
  summarise(n(),
            length(unique(scientificName)))

mamm %>%
  # Extract dates from registration numbers
  mutate(year = as.numeric(str_extract(mamm$catalogNumber, "\\d{4}"))) %>%
  # Extract only zoology records
  filter(collectionCode == "ZOO") %>%
  # Remove pre 1901 specimens
  filter(year < 1901 & !is.na(year) & year > 1750) %>%
  # Exclude without species names
  filter(!is.na(scientificName)) %>%
  # Exclude without locality data
  filter(!is.na(locality)) %>%
  # Get total number of specimens and species
  summarise(n(),
            length(unique(scientificName)))

fish %>%
  # Extract dates from registration numbers
  mutate(year = as.numeric(str_extract(fish$catalogNumber, "\\d{4}"))) %>%
  # Extract only zoology records
  filter(collectionCode == "ZOO") %>%
  # Remove pre 1901 specimens
  filter(year < 1901 & !is.na(year) & year > 1750) %>%
  # Exclude without species names
  filter(!is.na(scientificName)) %>%
  # Exclude without locality data
  filter(!is.na(locality)) %>%
  # Get total number of specimens and species
  summarise(n(),
            length(unique(scientificName)))
