#+++++++++++++++++++++++++++++++
#
# Title:  Trying reading in WFS file from AKTIS
#
# Author: Friederike Gebert
# 
# Date: 09.07.2025
#
#++++++++++++++++++++++++++++++++

library(sf)
library(ows4R)
library(httr)

aktis <- "https://isk.geobasis-bb.de/ows/atkisbdlm_sf_wfs?REQUEST=GetCapabilities&SERVICE=WFS"


st_layers(aktis)

layer_name <- "adv:AX_Landschaft"

landuse <- st_read(aktis, layer=layer_name)

plot(aktis["adv:AX_Landschaft"])

# does not work, empty layer

# try with bounding box

bbox <- st_bbox(c(
  xmin = 250000,  # west
  ymin = 5200000, # south
  xmax = 950000,  # east
  ymax = 6100000  # north
), crs = 25833)

landuse <- st_read(
  aktis,
  layer = "adv:AX_Landschaft",
  wfs_filter = bbox
)

# does not work, empty object

aktis <- WFSClient$new("https://isk.geobasis-bb.de/ows/atkisbdlm_sf_wfs?REQUEST=GetCapabilities&SERVICE=WFS", serviceVersion = "2.0.0")

features <- aktis$getFeatureTypes()
names(features)

# fails

aktis <- WFSClient$new(
  "https://isk.geobasis-bb.de/ows/atkisbdlm_sf_wfs?",
  serviceVersion = "2.0.0"
)

landuse <- WFSFeatureType$new(aktis, "adv:AX_Landschaft")

# Define a bounding box in EPSG:25833 (small area to test)
bbox <- c(375000, 5780000, 380000, 5785000)

# Try to get features
data <- landscape$getFeatures(bbox = bbox, srsName = "EPSG:25833")


res <- httr::GET("https://isk.geobasis-bb.de/ows/atkisbdlm_sf_wfs?SERVICE=WFS&REQUEST=GetCapabilities&VERSION=2.0.0")
httr::status_code(res)  # should be 200
content <- httr::content(res, as = "text", encoding = "UTF-8")
cat(substr(content, 1, 1000))  # print start of XML document

packageVersion("ows4R")

aktis <- WFSClient$new(
  "https://isk.geobasis-bb.de/ows/atkisbdlm_sf_wfs?",
  serviceVersion = "1.1.0"
)

features <- aktis$getFeatureTypes()
names(features)
# fails

landuse <- WFSFeatureType$new(aktis, "adv:AX_Landschaft")

wfs_url <- paste0(
  "https://isk.geobasis-bb.de/ows/atkisbdlm_sf_wfs?",
  "service=WFS&version=1.1.0&request=GetFeature",
  "&typename=adv:AX_Landschaft",
  "&srsname=EPSG:25833",
  "&bbox=375000,5780000,380000,5785000,EPSG:25833"
)

landscape_sf <- st_read(wfs_url)
landscape_sf <- st_read(wfs_url, layer = "adv:AX_Landschaft")
landscape_sf <- st_read(wfs_url, layer = "AX_Landschaft")

# the layer is empty
# r is overwhelmed
