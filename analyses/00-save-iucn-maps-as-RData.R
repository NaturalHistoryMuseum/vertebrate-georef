#--------------------------------------------------------------
# Reading in IUCN map data
# These files are large and annoying to store in a non .Rdata
# object. This code shows how to get from the downloaded maps
# to the .Rdata object in the repo
#
# Natalie Cooper 2017
#-----------------------------------------------------
# Setup
#-----------------------------------------------------
# Set working directory as the folder for this project
# or open a new project there

# Load libraries
library(rgdal)

# If you get errors in rgdal
# Try this link, the solution is at the bottom 
# https://github.com/OSGeo/proj.4/issues/351

#---------------------------------------------
# Read in maps 
#---------------------------------------------
maps <- readOGR("AMPHIBIANS")
# This is a folder containing all the files that come from IUCN
# downloads from http://www.iucnredlist.org/technical-documents/spatial-data

#---------------------------------------------
# Save file
#---------------------------------------------
save(maps, file = "iucn-amphibian-maps.Rdata")