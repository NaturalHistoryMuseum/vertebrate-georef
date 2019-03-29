library(rredlist)
library(tidyverse)

#usethis::edit_r_environ(IUCN_REDLIST_KEY = 'youractualkeynotthisstring')

mam <- rl_comp_groups('mammals')
write_csv(mam, path = here("data\IUCN-mammals.csv"))

bird <- rl_comp_groups('birds')
write_csv(bird, path = here("data\IUCN-birds.csv"))