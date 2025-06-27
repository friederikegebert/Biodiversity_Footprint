#++++++++++++++++++++++++++++++++++++++++++++++++
#
# Workflow: how to get biodiversity info for specific land use type
# Example Ecossystem types of Europe (EUNIS) and GBIF
#
# Author: Friederike Gebert
#
# Date: 27.06.2025
#
#+++++++++++++++++++++++++++++++++++++++++++++++



# setwd()

library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgbif)
library(tidyverse)

ecos <- rast("eea_r_3035_100_m_etm-terrestrial-r_2012_v3-1_r00.tif")

plot(ecos)
str(ecos)

# crop to Germany ####

# download Germany country polygon from rnaturalearth

germany_sf <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")

# transform Germany polygon to CRS EPSG 3035

germany_sf <- st_transform(germany_sf, crs = crs(ecos))

# convert to SpatVector for cropping with terra

germany_vect <- vect(germany_sf)

# crop ecos to Germany

r_crop <- crop(ecos, germany_vect)

# maks to exact German borders
germ <- mask(r_crop, germany_vect)

plot(germ)


germ$source

# overlay with data from gbif; example Coleoptera ####

# get taxon key for Coleoptera

search_results <- name_backbone("Coleoptera")

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


coleop <- occ_download(
  pred_and(
    pred("taxonKey", 1470),        # Coleoptera
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

coleop_d <- occ_download_get('0071922-250525065834625') %>%
  occ_download_import()


# extract occurrence data

coleop_d <- coleop_d %>%
  select(decimalLongitude, decimalLatitude) %>%
  filter(!is.na(decimalLongitude) & !is.na(decimalLatitude))

# convert to sf object

coleop_sf <- st_as_sf(coleop_d, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


# reproject coleop_sf to EPSG 3035

coleop_sf_proj <- st_transform(coleop_sf, crs = crs(ecos))

plot(germ)
plot(coleop_sf_proj$geometry, add = TRUE, col = "deeppink", pch = 20, cex = 0.6)


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

plot(forest)
plot(coleop_vect, add = TRUE, col = "red", pch = 20, cex = 0.7)
# # extract raster values
# 
# values_at_points <- terra::extract(germ, vect(coleop_sf_proj))
# 
# coleop_with_values <- cbind(coleop, values_at_points)

