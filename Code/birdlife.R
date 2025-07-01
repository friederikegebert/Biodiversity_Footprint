
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgbif)
library(tidyverse)

birds <- "BOTW_2024_2.gpkg"
data <- st_read(birds)
print(data)
plot(data)

saveRDS(data, file="birds_distributions.rds")

birds <- readRDS("birds_distributions.rds")

# check layers
st_layers(birds)

birds_sf <- st_read(birds, layer ="all_species")

st_write(birds_sf, "birds_allsp.gpkg", layer = "all_species")
birds_sf <- st_read("birds_allsp.gpkg", layer = "all_species")

str(birds_sf)
names(birds_sf)
head(birds_sf)
unique(birds_sf$sci_name)

unique(birds_sf$presence)
# plot(birds_sf["sci_name"])

# filter to native, extant, resident 

birds_subset <- birds_sf %>% 
  filter(origin ==1, presence ==1, seasonal==1)

# cast geometry
birds_sf_fixed <- st_cast(birds_sf, "MULTIPOLYGON", warn = FALSE)


crop_to_germany_raster <- function(data){ 
germany_sf <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")
germany_sf <- st_transform(germany_sf, crs = crs(data))
germany_vect <- vect(germany_sf)
r_crop <- crop(data, germany_vect)
germ <- mask(r_crop, germany_vect)
plot(germ)
return(germ)
}

crop_to_germany_vector <- function(data){ 
  germany_sf <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")
  germany_sf <- st_transform(germany_sf, crs = crs(data))
  germ <- st_intersection(data, germany_sf)
  plot(st_geometry(germ))
  return(germ)
}

crop_to_germany_vector(birds_sf_fixed)



# Check which geometries are invalid
invalid_idx <- which(!st_is_valid(birds_sf_fixed))

length(invalid_idx)  # How many?

# Option 1: Make geometries valid
birds_sf_fixed$geometry <- st_make_valid(birds_sf_fixed$geometry)

# Option 2: Filter out invalid geometries (if making valid fails)
birds_sf_valid <- birds_sf_fixed[st_is_valid(birds_sf_fixed), ]
