# ---- Setting up the environment ----

# Read the CSV file containing county data
df <- read.csv("data/SVI_2020_US_county.csv")

# Load required libraries
library(ggmap)  # For mapping
library(spdep)  # For spatial dependence
library(spatialreg)  # For spatial regression

# Load libraries for obtaining county geometries and FIPS codes
library(tigris)
library(dplyr)


# ---- Simple Linear Regression ----

# Create simple linear model that doesn't include spatial dependence
model <- lm(RPL_THEMES ~ AREA_SQMI + E_TOTPOP, data = df)

# Print summary of the regression results
summary(model)

# Show the residuals
plot(model$residuals)

# Add residuals from the simple linear model to the data frame
df$RPL_THEMES_RESIDUALS <- model$residuals

# ---- Tests for Spatial Dependence ----

## ---- Map of Residuals ----

# Get county FIPS codes from the data
county_fips <- df$FIPS

# Get county geometries using tigris package
county_geom <- counties(cb = TRUE) %>%
    filter(GEOID %in% county_fips)

# Add geometry to the data frame
df <- merge(df, county_geom[, c("geometry","GEOID"), drop = FALSE], by.x = "FIPS", by.y = "GEOID")

# Create map of continental US counties
ggplot() +
    geom_sf(data = sf::st_as_sf(df), aes(fill = RPL_THEMES_RESIDUALS), color = "black") +
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
    theme_void()

## ---- Moran's I ----

# Get county centroids
county_centroids <- st_centroid(county_geom)

# Extract latitude and longitude from county centroids
geocodes <- st_coordinates(county_centroids)

# Convert geocodes to a data frame
geocodes_df <- as.data.frame(geocodes)

# Combine county geometries with geocodes data frame
geocodes_df <- cbind(geocodes_df, county_geom)

# Merge geocodes_df back with the original data frame
df <- merge(df, geocodes_df, by.x = "FIPS", by.y = "GEOID", suffixes = c("", ".geocodes"))

# Create spatial weights matrix using spdep function
coords <- cbind(df$X, df$Y)
weights <- dnearneigh(coords, d1 = 0, d2 = Inf, k = 5)

# Convert weights to a matrix
weights_matrix <- as.matrix(weights)

# Create a spatial lag variable
weights_listw <- nb2listw(weights_matrix, style = "W")

# Perform Global Moran's test for outcome
moran_result <- moran.mc(df$RPL_THEMES, weights_listw, nsim = 999)

# Get the p-value
p_value <- moran_result$p.value
p_value

# Perform Global Moran's test for residuals
moran_result <- moran.mc(df$RPL_THEMES_RESIDUALS, weights_listw, nsim = 999)

# Get the p-value
p_value <- moran_result$p.value
p_value

# -------------------------------------------- Spatial Regression --------------------------------------------

# Perform spatial regression using spatialreg function
model <- lagsarlm(df$RPL_THEMES ~ df$AREA_SQMI + df$E_TOTPOP, data = df, listw = weights_listw)

# Print summary of the regression results
summary(model)

