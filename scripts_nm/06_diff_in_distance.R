##==============================================================================
## Project: QuEST
## This script is to calculate distance between two sites
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##################
#### Packages ####
##################

library(geosphere)

#####################
#### Coordinates ####
#####################
# One Barologger can be used to compensate all Leveloggers in a 20 mile (30 km) radius 
# and/or with every 1000 ft. (300 m) change in elevation.
# If:
# upper air pt location <- (35.78002, -105.77377), elevation = 3,337 m
# USF20pt location <- (35.73099, -105.79001), elevation = 2,668 m

# Calculate distance in meters
# distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
distm(c(-105.77377, 35.78002), c(-105.79001, 35.73099), fun = distHaversine)

3337 - 2668

# distance difference: 5651.729 m
# elevation difference" 669 m