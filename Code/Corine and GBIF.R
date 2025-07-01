#++++++++++++++++++++++++++++++++++++++++++++++++
#
# Workflow: how to get biodiversity info for specific land use type
# Example CORINE and GBIF butterflies
#
# Author: Friederike Gebert
#
# Date: 27.06.2025
#
#+++++++++++++++++++++++++++++++++++++++++++++++



# setwd()
# "C:/Users/fgebert/Senckenberg Dropbox/Friederike Gebert/Senckenberg Gelnhausen/BIodiversity Footprint/datasets/CORINE/u2018_clc2018_v2020_20u1_raster100m/u2018_clc2018_v2020_20u1_raster100m/DATA"

library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgbif)
library(tidyverse)
library(leaflet)
library(raster)


# with corine as raster ####
corine <- rast("U2018_CLC2018_V2020_20u1.tif")
saveRDS(corine, "corine.rds")

plot(corine)

crop_to_germany_raster <- function(data){ 
  germany_sf <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")
  germany_sf <- st_transform(germany_sf, crs = crs(data))
  germany_vect <- vect(germany_sf)
  r_crop <- crop(data, germany_vect)
  germ <- mask(r_crop, germany_vect)
  # plot(germ)
  return(germ)
}


germ <- crop_to_germany_raster(corine)


# crop to land use type "Mixed forest"

germ
cats <- cats(germ)
print(cats)
str(cats)

cats_df <- cats(germ)[[1]]
unique(cats_df$LABEL3)
subset(cats_df, LABEL3 == "Mixed forest")

forest_values <- cats_df$Value[cats_df$LABEL3 == "Mixed forest"]

forest_mask <- classify(germ, rcl = cbind(forest_values, 1), others = NA)


# get map just with forest

forest <- mask(forest_mask, germany_vect)
plot(forest, col="orange")

# create interactive map

## convert to raster layer
forest_raster <- raster(forest)  

pal <- function(x) {
  # For all non-NA values, return "orange", else transparent
  ifelse(!is.na(x), "orange", "transparent")
}

leaflet() %>%
  addTiles() %>%
  addRasterImage(forest_raster, colors = pal, opacity = 0.7) %>%
  addLegend(colors = "orange", labels = "Mixed Forest", title = "Land Cover") %>%
  addPolygons(data = germany_vect, color = "blue", weight = 2, fill = FALSE)







# overlay with data from gbif; example Butterflies ####


occ_count(country="DE")

# get taxon key for Papilionoidea
unique(gbif_data$family)

search_results <- name_backbone("Lepidoptera")

# Extract the taxonKey

taxon_key <- search_results$usageKey
taxon_key

# the taxon key is 1470

# get GBIG entries for all beetles in Germany
# needs gbif account

# provide GBIG login details

user <- "your_gbif_username"
pwd  <- "your_gbif_password"
email <- "your_email@example.com"



user <- "friederike.gebert88"
pwd <- "FG197988*Ext*"
email <- "friederike.gebert@senckenberg.de"


lepi <- occ_download(
  pred_and(
    pred("taxonKey", 797),        # Lepidoptera
    pred("country", "DE"),         # Germany
    pred("hasCoordinate", TRUE)
  ),
  user = user,
  pwd = pwd,
  email = email
)

# this takes a while...

# you get this info

#<<gbif download>>
# Your download is being processed by GBIF:
#   https://www.gbif.org/occurrence/download/0071922-250525065834625
# Most downloads finish within 15 min.
# Check status with
# occ_download_wait('0071922-250525065834625')
# After it finishes, use
# d <- occ_download_get('0071922-250525065834625') %>%
#   occ_download_import()
# to retrieve your download.
# Download Info:
#   Username: friederike.gebert88
# E-mail: friederike.gebert@senckenberg.de
# Format: DWCA
# Download key: 0071922-250525065834625
# Created: 2025-06-27T14:33:53.666+00:00
# Citation Info:  
#   Please always cite the download DOI when using this data.
# https://www.gbif.org/citation-guidelines
# DOI: 
#   Citation:
#   GBIF Occurrence Download https://www.gbif.org/occurrence/download/0071922-250525065834625 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-06-27

occ_download_wait('0084606-250525065834625')

lepi_d <- occ_download_get('0084606-250525065834625') %>%
  occ_download_import()

saveRDS(lepi_d, file="lepi_d.rds")
names(lepi_d)

unique(lepi_d$year)


# butterflies in Germany: 7 families: 

# Hesperiidae (Dickkopffalter)
# Papilionidae (Ritterfalter)
# Pieridae (Weißlinge)
# Lycaenidae (Bläulinge)
# Riodinidae (Würfelfalter)
# Nymphalidae (Edelfalter)
# Satyridae (Augenfalter)


# filter so that 2 years plus minus corine: years 2016, 2017, 2018, 2019, 2020

lepi_d_2017_2020 <- lepi_d %>% 
  filter(year %in% c(2017, 2018, 2019, 2020))

names(lepi_d_2017_2020)
unique(lepi_d_2017_2020$family)

# filter to butterfly families

butterfly_families <- c("Hesperiidae", "Papilionidae", "Pieridae", 
                        "Lycaenidae", "Riodinidae", "Nymphalidae", "Satyridae")

lepi_d_2017_2020_but <- lepi_d_2017_2020 %>% 
  filter(family %in% butterfly_families)

saveRDS(lepi_d_2017_2020_but, file="lepi_d_2017_2020_but.rds")



# extract occurrence data

but <- lepi_d_2017_2020_but %>%
  dplyr::select(decimalLongitude, decimalLatitude, species, family) %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# convert to sf object

but_sf <- st_as_sf(but, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


# reproject but_sf to EPSG 3035

but_sf_proj <- st_transform(but_sf, crs = crs(germ))

plot(germ)
plot(but_sf_proj$geometry, add = TRUE, col = "deeppink", pch = 20, cex = 0.6)

saveRDS(germ, file="germ.rds")
saveRDS(but_sf_crop, file="but_sf_crop.rds")
# crop but to germ

# Convert raster extent to sf polygon
germ_extent <- ext(germ) %>%
  as.polygons() %>%
  st_as_sf()

# Ensure both use the same CRS (with st_transform)
# Now crop the points
but_sf_crop <- st_crop(but_sf_proj, germ_extent)


# plot on Germany map

germ_df <- as.data.frame(germ, xy = TRUE, na.rm = TRUE)
names(germ_df)[3] <- "land_use"

# nicer Germany map
ggplot() +
  geom_raster(data = germ_df, aes(x = x, y = y, fill = land_use)) +
  scale_fill_viridis_d(name = "Land Use")+
  coord_sf() +
  guides(fill ="none")+
  theme_minimal()


ggplot() +
  geom_raster(data = germ_df, aes(x = x, y = y, fill = land_use)) +
  scale_fill_viridis_d(name = "Land Use") +  # or use scale_fill_manual for custom colors
  geom_sf(data = but_sf_crop, color = "deeppink", size = 0.6, alpha = 0.7) +
  coord_sf() +
  guides(fill ="none")+
  theme_minimal() +
  labs(title = "Butterflies of Germany",
       x = "Longitude", y = "Latitude")






plot(forest, col="orange")
plot(but_sf_proj$geometry, add = TRUE, col = "deeppink", pch = 20, cex = 0.6)


# crop to land use type "Woodland and forest"

germ
cats <- cats(germ)
print(cats)
str(cats)

cats_df <- cats(germ)[[1]]
unique(cats_df$MAES_L2)
subset(cats_df, MAES_L2 == "Woodland and forest")

forest_values <- cats_df$Value[cats_df$MAES_L2 == "Woodland and forest"]

forest_mask <- classify(germ, rcl = cbind(forest_values, 1), others = NA)


# get map just with forest

forest <- mask(forest_mask, germany_vect)
plot(forest)


# get GBIF points just for forest land use type

# just making sure crs is the same
coleop_sf_proj <- st_transform(coleop_sf_proj, crs(forest))

# 
coleop_vect <- vect(coleop_sf_proj)

coleop_extracted <- terra::extract(forest, coleop_vect)

coleop_vect$forest <- coleop_extracted[,2]

coleop_forest <- coleop_vect[!is.na(coleop_vect$forest), ]



plot(forest)
plot(coleop_forest, add = TRUE, col = "red", pch = 20, cex = 0.5)

# calculate area of forest

cell_areas <- cellSize(forest, unit="m")

forest_areas <- mask(cell_areas, forest)

# total forest area in m2

forest_areas_m2 <- global(forest_areas, "sum", na.rm=T)[1,1]

# convert to km2

forest_areas_km <-  forest_areas_m2/1e6

# calculate number of GBIF records for forest

length(coleop_forest)

# inspect spatial accuracy of GBIF records

unique(coloep_forest$coordinateUncertaintyInMeters)
summary(coleop_forest$coordinatePrecision)


# # extract raster values
# 
# values_at_points <- terra::extract(germ, vect(coleop_sf_proj))
# 
# coleop_with_values <- cbind(coleop, values_at_points)

