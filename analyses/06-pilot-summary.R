# Read in all gbif records
gbif <- read_csv("data/all-gbif.csv")
glimpse(gbif)

# Remove post 1900 records and those with no year
gbif <- 
  gbif %>%
  filter(year < 1901 & !is.na(year))
glimpse(gbif)

# Summarise by class
gbif %>%
  group_by(class) %>%
  summarise(n(),
            min(year))

# How many species?
gbif %>%
  group_by(class) %>%
  summarise(length(unique(species)))

