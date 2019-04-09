library(rredlist)
library(tidyverse)
library(here)

#IUCN_REDLIST_KEY = '2745adcf154b3539fae004c16f1bd9a6ea3a5138cf466a00e33b0beac666f8c2')

# Extract IUCN categories
mam <- rl_comp_groups('mammals', key = '2745adcf154b3539fae004c16f1bd9a6ea3a5138cf466a00e33b0beac666f8c2')
bird <- rl_comp_groups('birds', key = '2745adcf154b3539fae004c16f1bd9a6ea3a5138cf466a00e33b0beac666f8c2')

# Tidy the data
iucn <- rbind(mam$result, bird$result) 
iucn <- 
  iucn %>%
  filter(is.na(subspecies)) %>%
  select(scientific_name, category) %>%
  distinct()

# Save  
write_csv(iucn, path = here("data/IUCN-data.csv"))

