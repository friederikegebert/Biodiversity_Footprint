---
title: "Copernicus_land_cover_map"
output: html_document
date: "2025-03-07"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Overview

This script lays down the processing steps required to produce a tailored land cover map from the processed Copernicus datasets for the purpose of employing an urban ecological modeling framework. A prior decisions has to be taken on the urban area of interest for which the land cover map should be produced. Correspondingly the four spatial data layers required (CLC+Backbone (10 m), Urban Atlas DEM (10m), the Urban Atlas dataset (vector) and the Water and Wetness Layer (10 m)) should already have been obtained via the Copernicus Land Monitoring Service (CLMS) website. On the CLMS website, a registered account is required and datasets can be downloaded from the data viewer by defining the required area extent. Preferably, the regions should be selected from the area selection and then downloaded via the download queue. The download of the specified product area has to be prepared by the CLMS which can take from a few minutes to up to 24 hours depending on the product and area chosen. In addition to that a boundary shapefile of the selected urban area is required for efficient processing.
The spatial resolution is 10 m This script utilizes the urban area of Munich as an exemplary urban area of interest.

## Packages
### Install packages if required

```{r include = FALSE}
RequiredPackages <- c("terra", "sf", "dplyr")
install.packages(setdiff(RequiredPackages, rownames(installed.packages())))
```

### Load packages
```{r include = FALSE}
library(RequiredPackages)
```

## Custom environment
These are directories and names that have to be defined by each user depending on the used machine and city.

```{r}
#PathWD            <- "patch to working directory" # typically "C:/Users/Name ..."
selectedUrbanArea <- "Munich" # change according to your urban area of interest 

# Load directories as well as the urban boundary 
#setwd(PathWD)
print(paste0("Working directory is: ", getwd()))

```

## Setup folders with spatial data
### Download links of spatial data 
1 CLC+Backbone (raster data): https://land.copernicus.eu/en/products/clc-backbone/clc-backbone-2021
2 Urban Atlas DEM (raster data): https://land.copernicus.eu/en/products/urban-atlas/building-height-2012
3 Urban Atlas (vector data): https://land.copernicus.eu/en/products/urban-atlas/urban-atlas-2018
4 Water and Wetness Area (raster data): https://land.copernicus.eu/en/products/high-resolution-layer-water-and-wetness/water-and-wetness-status-2018

### SpatialData subfolder
The Copernicus rasters and a shapefile with the study area has to be in the "SpatialData" subfolder of the current working directory.

```{r}
setwd("C:/Users/maxim/OneDrive/Documentos/")
SpatialDataPath <- paste0(getwd(),"/SpatialData/") # the four spatial datasets should be in this directory
ListSpatialData <- list.files(SpatialDataPath, pattern = "\\.(shp|tif|gpkg)$", full.names = TRUE, recursive = TRUE)

NRequiredFiles  <- 5

# check whether the vector file with city boundary and all Copernicus raster datasets are in the SpatialData folder
print(paste0("Spatial data available for processing: ", ListSpatialData))
if (NRequiredFiles > length(ListSpatialData)) {
  print(paste0("There are files missing. Check out whether you included all required files in the SpatialData subfolder."))
}

```

## Define custom functions
Function conditional_replace is required to replace cells with a specific value of one layer with the values of another layer.
```{r include = FALSE}
conditional_replace <- function(rasters) { # function for raster layer overlay / reclassification
  x <- rasters[[1]]  
  y <- rasters[[2]] 
  
  ifelse(x == ClassValueToReplace, y, x)
}

conditional_addition <- function(rasters) { # function for raster layer overlay / reclassification
  x <- rasters[[1]]  
  y <- rasters[[2]] 
  
  ifelse(x == ClassValueToAdd, x, y)
}
```

## Load spatial data
Load the different rasterfiles and shapefile to be processed
```{r include = FALSE}
CityBoundary  <- vect(paste0(SpatialDataPath, "/Munich_border_3035.shp")) # City boundary replace with desired city 

CLCplus        <- rast(grep("CLCplus.*\\.tif$", ListSpatialData, value = TRUE)) # CLCplus_2018 raster 
UrbanAtlasDEM  <- rast(grep("DHM.*\\.tif$", ListSpatialData, value = TRUE)) # Urban Atlas DEM raster
WaterRaster    <- rast(grep("WAW.*\\.tif$", ListSpatialData, value = TRUE))

UrbanAtlasGPKG <- grep("UA.*\\.gpkg$", ListSpatialData, value = TRUE)
GPKGLayers     <- st_layers(UrbanAtlasGPKG)
StreetVector   <- st_read(grep( "UA.*\\.gpkg$", ListSpatialData, value = TRUE), layer = GPKGLayers$name[1])
StreetVector   <- vect(StreetVector)
```

Target Specifications for modeling
```{r include = FALSE}
TargetCRS        <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
TargetResolution <- c(10, 10) # 10 m resolution 
TargetRes        <- 10
TargetExtent     <- ext(CLCplus)  

TemplateRaster   <- rast(extent = TargetExtent, # use as template for geo formating of other rasters (projection, pixel size) 
                         resolution = TargetRes, 
                         crs = crs(TargetCRS))

# Reproject layers to target crs
CityBoundary     <- project(CityBoundary, TargetCRS)
CityBoundary     <- buffer(CityBoundary, width = 10)  # buffer to avoid later cropping of valid pixels

```

## Spatial data processing

### Process CLC+ backbone raster
```{r}
ReclassMatrixCLC <- matrix(c(
  1, 1,  # Sealed to Sealed and bare soil
  2, 31, # Woody – needle leaved trees
  3, 32, # Woody – Broadleaved deciduous trees
  4, 33, # Woody – Broadleaved evergreen trees
  5, 5,  # Low-growing woody plants (bushes, shrubs)
  6, 6,  # Permanent herbaceous (grasslands)
  7, 7,  # Periodically herbaceous (agriculture)
  8, 8,  # Lichens and mosses
  9, 9,  # Non- and sparsely-vegetated 
  10, 10 # Water
), ncol = 2, byrow = TRUE)

CLCplus <- classify(CLCplus, ReclassMatrixCLC)
CLCplus <- crop(CLCplus, CityBoundary)

# verify results
plot(CLCplus)
print(sort(unique(values(CLCplus))))
print("Possible values:")
print(sort(unique(ReclassMatrixCLC[, 2])))
print("If not all possible values occur in the created map, this is probably caused by a lack of this vegetation type in the selected spatial area.")


```

## Processsing Urban Atlas DEM
```{r}
ReclassMatrixBuildingHeight <- matrix(c(
  0, 255,
  1, 100, # Low Buildings (2-10 m)
  2, 200,
  3, 300,
  4, 400,
  5, 500, # Medium Buildings (10-20 m) 
  6, 600,
  7, 700, # High Buildings (20-368 m)
  8, 800,
  9, 900,
  10,1000
), ncol = 2, byrow = TRUE)

# Reclassify the DEM raster
UrbanAtlasDEM                                       <- classify(UrbanAtlasDEM, ReclassMatrixBuildingHeight)
values(UrbanAtlasDEM)[is.na(values(UrbanAtlasDEM))] <- 255
UrbanAtlasDEM                                       <- project(UrbanAtlasDEM, TemplateRaster, method = "near")

# Then perform the crop with the buffered extent
UrbanAtlasDEM                                       <- crop(UrbanAtlasDEM, CityBoundary)

# verify results
plot(UrbanAtlasDEM)
print(sort(unique(values(UrbanAtlasDEM))))
print("Possible values:")
print(sort(unique(ReclassMatrixBuildingHeight[, 2])))
print("If not all possible values occur in the created map, this is probably caused by a lack of this building height in the selected spatial area.")


```

### Processing Water and Wetness
```{r }
ReclassMatrixWater <- matrix(c(
  0, 255,
  1, 255,
  2, 255,
  3, 11, # only extracts permanent wetness
  4, 255
), ncol = 2, byrow = TRUE)

# Reclassify the DEM raster
WaterRaster                                      <- classify(WaterRaster, ReclassMatrixWater)
values(WaterRaster)[is.na(values(WaterRaster))]  <- 255 # 255 is in the following defined as the NoData Value
WaterRaster                                      <- project(WaterRaster, TemplateRaster, method = "near") 
WaterRaster                                      <- crop(WaterRaster, CityBoundary)

# verify results
plot(WaterRaster)
print(sort(unique(values(WaterRaster))))
print("Possible values:")
print(sort(unique(ReclassMatrixWater[, 2])))
print("If not all possible values occur in the created map, this is probably caused by a lack of this water body in the selected spatial area.")

```

### Processing Urban Atlas Roads
```{r }
# Rasterize streets obtained from Urban Atlas
StreetClasses     <- c("12210", "12220", "12230") # we need to extract the features where the code_2018 is those 

ExtractedStreets  <- StreetVector[StreetVector$code_2018 %in% StreetClasses, ]# Subset based on the 'code_2018' column
ExtractedStreets  <- buffer(ExtractedStreets, width = 8) # buffer to avoid loosing street area

# Check if the extraction was successful
print(nrow(ExtractedStreets)) # should indicate the number of extracted polygons / Lines
plot(ExtractedStreets)
print(unique(values(ExtractedStreets)))

# Rasterize the extracted polygons
StreetRaster     <- rasterize(ExtractedStreets, TemplateRaster, field = "code_2018", fun = 'max')

# Define reclassification matrix 
ReclassMatrixStreets <- matrix(c( # format: old value, new value
  0, 91, # Fast Transit Roads and Associated Lands
  1, 92, # Other Roads and Associated Lands
  2, 93  # Railways and Associated Lands
), ncol = 2, byrow = TRUE)

# Reclassify the street raster
StreetRaster                                                <- classify(StreetRaster, ReclassMatrixStreets)
values(StreetRaster)[is.na(values(StreetRaster))]           <- 255
StreetRaster                                                <- project(StreetRaster, TemplateRaster, method = "near")
StreetRaster                                                <- crop(StreetRaster, CityBoundary)

# verify the results
plot(StreetRaster)
print(sort(unique(values(StreetRaster))))
print("Possible values:")
print(sort(unique(ReclassMatrixStreets[, 2])))
print("If not all possible values occur in the created map, this is probably caused by a lack of this street type in the selected spatial area.")

```

## Raster calculation

### Building raster height calculation
```{r }
ClassValueToReplace <- 1 # Sealed class value that needs to be replaced
CLCplus_UADEM       <- terra::app(c(CLCplus, UrbanAtlasDEM), fun = conditional_replace) # Apply conditional replacement function

ClassValueToReplace <- 255 # pixels where no height was attributed should be replaced again by the CLC+Backbone dataset
CLCplus_UADEM       <- terra::app(c(CLCplus_UADEM, CLCplus),fun = conditional_replace) # Conduct replacement of no data values by initial CLC layer

# verify results
plot(CLCplus_UADEM) # merged raster layer of combined CLC plus and DEM raster data
print(sort(unique(values(CLCplus_UADEM))))
print("Expected values:")
ExpectedValues <- sort(c(unique(values(CLCplus)), unique(values(UrbanAtlasDEM))))
ExpectedValues <- ExpectedValues[ExpectedValues != 255]
print(ExpectedValues)
```

### Water replace
```{r }
ClassValueToAdd     <- 11 # permanent wet should be added to CLC+ + urban atlas 
CLCplus_UADEM_W     <- terra::app(c(WaterRaster, CLCplus_UADEM), fun = conditional_addition)

ClassValueToReplace <- 255 # NA class value that needs to be replaced
CLCplus_UADEM_W     <- terra::app(c(CLCplus_UADEM_W, CLCplus_UADEM), fun = conditional_replace)

# verify results
plot(CLCplus_UADEM_W)
print(sort(unique(values(CLCplus_UADEM_W))))
print("Expected values:")
ExpectedValues <- sort(c(unique(values(CLCplus)), unique(values(UrbanAtlasDEM)), unique(values(WaterRaster))))
ExpectedValues <- ExpectedValues[ExpectedValues != 255]
print(ExpectedValues)
```

### Street replace
```{r }
ClassValueToReplace <- 1 # Class value that needs to be replaced
CLCplus_UADEM_W_Str <- terra::app(c(CLCplus_UADEM_W, StreetRaster), fun = conditional_replace)
plot(CLCplus_UADEM_W_Str)

ClassValueToReplace <- 255 # Class value that needs to be replaced
CLCplus_UADEM_W_Str <- terra::app(c(CLCplus_UADEM_W_Str, CLCplus_UADEM_W), fun = conditional_replace)

# verify results
plot(CLCplus_UADEM_W_Str)
print(sort(unique(values(CLCplus_UADEM_W_Str))))
print("Expected values:")
ExpectedValues <- sort(c(unique(values(CLCplus)), unique(values(UrbanAtlasDEM)), unique(values(WaterRaster)), unique(values(StreetRaster))))
ExpectedValues <- ExpectedValues[ExpectedValues != 255]
print(ExpectedValues)

#### 6.4 Crop to final extent
CLCplus_UADEM_W_Str <- mask(CLCplus_UADEM_W_Str, CityBoundary) # final mask to only export values within city boundaries
plot(CLCplus_UADEM_W_Str)
```

## Export final raster
```{r include = FALSE}
# Create integer values for raster for better export
values(CLCplus_UADEM_W_Str) <- as.integer(values(CLCplus_UADEM_W_Str))

# Create labels table to later join it to the raster in ArcGISPro or QGIS
landcover_labels <- data.frame(
  value = c(0, 1, 5, 6, 7, 8, 9, 
            10, 11, 31, 32, 33,
            91, 92, 93, 
            100, 200, 300, 400, 500, 
            600, 700, 800, 900, 1000),    
  label = c("NoData","Sealed", "Shrubs and Bushes", "Grassland", "Agriculture", "Lichens and Mosses",  "Non- and Sparsely-vegetated",
            "Water", "Wetland","Needle-leaved Trees", "Broadleaved Deciduous Trees", "Broadleaved Evergreen Trees",
            "Fast Transit Roads and Associated Land", "Other Roads and Associated Land","Railways and Associated Land", 
            "Building (2-4 m)", "Building (4-6 m)", "Building (6-8 m)", "Building (8-10 m)", "Building (10-15 m)", 
            "Building (15-20 m)", "Building (20-30 m)", "Building (30-60 m)","Building 60-100 m", "Building (100-368 m)") 
)

writeRaster(CLCplus_UADEM_W_Str, paste0(SpatialDataPath,"Copernicus_landcover_map", selectedUrbanArea, ".tif"), overwrite = TRUE)
write.csv(landcover_labels, paste0(SpatialDataPath,"Copernicus_landcover_labels.csv"), row.names = FALSE)

```
