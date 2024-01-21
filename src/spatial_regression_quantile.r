# Read the CSV file containing county data
df <- read.csv("data/SVI_2020_US_county.csv")

# Load required libraries
library(ggmap)  # For mapping
library(tigris)  # For obtaining county geometries and FIPS codes
library(dplyr)  # For data manipulation
library(splm)  # For spatial panel data models
library(quantreg)  # For quantile regression
library(sf)  # For spatial data
library(spdep)  # For spatial weights matrix

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

# Create spatial weights matrix using splm function
coords <- cbind(df$X, df$Y)
weights <- dnearneigh(coords, d1 = 0, d2 = Inf, k = 5)

# Convert weights to a matrix
weights_matrix <- as.matrix(weights)

# Create a spatial lag variable
weights_listw <- nb2listw(weights_matrix, style = "W")
df$RPL_THEMES_LAG <- lag.listw(weights_listw, df$RPL_THEMES)

# Calculate correlation between RPL_THEMES_LAG and RPL_THEMES
correlation <- cor(df$RPL_THEMES_LAG, df$RPL_THEMES)
correlation

# Perform quantile regression using rqss function with weights_matrix
model <- rqss(RPL_THEMES ~ AREA_SQMI + E_TOTPOP, data = df)

# Print summary of the regression results
summary(model)

# Perform quantile regression using rqss function with weights_matrix
model <- rqss(RPL_THEMES_LAG ~ AREA_SQMI + E_TOTPOP, data = df)

# Print summary of the regression results
summary(model)
