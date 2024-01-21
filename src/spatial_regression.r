# Read the CSV file containing county data
df <- read.csv("data/SVI_2020_US_county.csv")

# Load required libraries
library(ggmap)  # For mapping
library(spdep)  # For spatial dependence

# Load libraries for obtaining county geometries and FIPS codes
library(tigris)
library(dplyr)

# Get county FIPS codes from the data
county_fips <- df$FIPS

# Get county geometries using tigris package
county_geom <- counties(cb = TRUE) %>%
    filter(GEOID %in% county_fips)

# Get county centroids
county_centroids <- st_centroid(county_geom)

# Extract latitude and longitude from county centroids
geocodes <- st_coordinates(county_centroids)

# Convert geocodes to a data frame
geocodes_df <- as.data.frame(geocodes)

# Combine county geometries with geocodes data frame
geocodes_df <- cbind(geocodes_df, county_geom)

# Merge geocodes_df back with the original data frame
df <- merge(df, geocodes_df, by.x = "FIPS", by.y = "GEOID")

# Create spatial weights matrix using dnearneigh function
coords <- cbind(df$X, df$Y)
weights <- dnearneigh(coords, 0, 100)

# Create a spatial lag variable
df$RPL_THEMES <- lag.listw(weights, df$RPL_THEMES)

# Perform spatial regression using errorsarlm function
model <- errorsarlm(RPL_THEMES ~ AREA_SQMI + E_TOTPOP, data = df, weights = weights)

# Print summary of the regression results
summary(model)
