# Pilot analyses on amphibian data

# Load libraries
library(tidyverse)
library(ggmap)
library(viridisLite)

# Read in the data
gbif <- read_csv("data/amphibians-gbif.csv")
glimpse(gbif)

# What's the earliest GBIF vs NHM record?
gbif %>%
  group_by(source, family) %>%
  summarise(min(year))

# Make violin plots
ggplot(gbif2, aes(x = family, y = year)) + 
  geom_violin() +
  facet_wrap(~source, ncol = 1) +
  theme_bw() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/violin.png", height = 5, width = 8, 
       units = c("cm"))