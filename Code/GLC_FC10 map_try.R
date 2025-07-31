
#++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Land cover map GLC_FC10
#

# Author: Friederike Gebert
#
# Date: 28.07.2025
# 
#++++++++++++++++++++++++++++++++++++++++++++++++++++


# Dataset:

# https://zenodo.org/records/14729665

# Germany approxmiate bounding box:
# Longitude (E): E005–E015
# Latitude (N): N047–N055

# setwd("C:/Users/fgebert/Senckenberg Dropbox/Friederike Gebert/Senckenberg Gelnhausen/BIodiversity Footprint/datasets/GLC_FC10_2023")

library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(leaflet)
library(dplyr)
library(geodata)
library(mapview)

tile_E005N45 <- rast("GLC_FCS10_2023_E005N45.tif")
tile_E005N50 <- rast("GLC_FCS10_2023_E005N50.tif")
tile_E005N55 <- rast("GLC_FCS10_2023_E005N55.tif")
tile_E010N45 <- rast("GLC_FCS10_2023_E010N45.tif")
tile_E010N50 <- rast("GLC_FCS10_2023_E010N50.tif")
tile_E010N55 <- rast("GLC_FCS10_2023_E010N55.tif")
tile_E015N45 <- rast("GLC_FCS10_2023_E015N45.tif")
tile_E015N50 <- rast("GLC_FCS10_2023_E015N50.tif")
tile_E015N55 <- rast("GLC_FCS10_2023_E015N55.tif")

combined_map <- merge(tile_E005N45,tile_E005N50,tile_E005N55,
                      tile_E010N45,tile_E010N50,tile_E010N55,
                      tile_E015N45,tile_E015N50,tile_E015N55 )

# Warnmeldung: [merge] Estimated disk space needed without compression: 103GB. Available: 34 GB

plot(combined_map)

combined_map <- mosaic(tile_E005N45,tile_E005N50,tile_E005N55,
                      tile_E010N45,tile_E010N50,tile_E010N55,
                      tile_E015N45,tile_E015N50,tile_E015N55, filename = "combined_map.tif", overwrite =T )

plot(combined_map)
# covers much more than Germany

crs(combined_map)

# load and reproject Germany

Germany_reproject <- function(data){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  germany <- world[world$name == "Germany", ]
  germany_proj <- st_transform(germany, crs = crs(data))
  return(germany_proj)
}

Germany <- Germany_reproject(combined_map)

plot(Germany$geometry, add=T, border="black", lwd=4)

Germany_plot <- function(data){
  Germany <- Germany_reproject(data)
  Germany_vect <- vect(Germany)
  Germany_crop <- crop(data, Germany_vect)
  Germany_masked <- mask(Germany_crop, Germany_vect)
  plot(Germany_masked)
  plot(Germany_vect, add=T, border="black", lwd=2)
  }

Germany_plot(combined_map)


plot_only_Germany <- function(data){
  world <- ne_countries(scale = "medium", returnclass = "sf")
  germany <- world[world$name == "Germany", ]
  germany_proj <- st_transform(germany, crs = crs(data))
  germany_vect <- vect(germany_proj)
  germany_crop <- crop(data, germany_vect)
  germany_masked <- mask(germany_crop, germany_vect)
  plot(germany_masked)
  plot(germany_vect, add=T, border="black", lwd=2)
  return(germany_masked)
  }

# load combined map

combined_map <- rast("combined_map.tif")
plot(combined_map)

germany_masked <- plot_only_Germany(combined_map)

writeRaster(germany_masked, "germany_masked.tif", overwrite =T)
# get legend to colours

# is.factor(germany_masked)
# cats <- cats(germany_masked_fact)
# print(cats)
# 
# coltab(germany_masked)

germany_masked <- rast("germany_masked.tif")
plot(germany_masked)
summary(germany_masked)

germany_masked_fact <- as.factor(germany_masked)
summary(germany_masked_fact)
levels(germany_masked_fact)


# clean factors so that each factor only appears once

levs <- levels(germany_masked_fact)[[1]]
levs$remapped <- as.numeric(levs$remapped)
levs
levs_clean <- levs[!duplicated(levs$ID), ]

# does not work

levels_germany_masked <- data.frame(ID = c(11, 12, 20, 51, 52, 61, 62, 71, 72, 81, 82, 91, 92, 121, 122,
                                           130, 140, 181, 182, 183, 184, 185, 186, 187, 191, 192, 150, 200, 210, 220),
                                    class = c("Herbaceous rainfed cropland", "Tree or shrub covered rainfed cropland", "Irrigated cropland",
                                              "Closed evergreen broadleaved forest", "Open evergreen broadleaved forest", "Closed deciduous broadleaved forest",
                                              "Open deciduous broadleved forest", "Closed evergreen neadleleaved forest", "Open evergreen needleleaved forest",
                                              "Closed deciduous needleleaved forest", "open deciduous needleleaved forest", "Closed mixed-leave forest",
                                              "Open mixed leaved forest", "Evergreen shrubland", "Deciduous shrubland", "Grassland",
                                              "Lichens and mosses", "Swamp", "Marsh", "Lake-river flat", "Saline", "Mangrove forest", "Salt marsh", "Tidal flat",
                                              "Urban impervious surfaces", "Rural impervious surfaces", "Spare vegetation", "Bare areas", "Water", "Permanent ice and snow"))
levels(germany_masked) <- list(levels_germany_masked)

n_classes <- nrow(levels_germany_masked)

# Generate colorblind-friendly colors using viridis
cols <- rev(viridis(n_classes))


# Create a named vector to map IDs to colors
id_colors <- setNames(cols, levels_germany_masked$ID)

# increase margins around plot# 

par(mar=c(5,4,4,10))

# Plot raster with these colors
plot(germany_masked, col = cols, legend = FALSE)

# Add legend with class names
legend("topright", legend = levels_germany_masked$class, fill = cols, cex = 0.7, bty = "n", xpd =T)



# I don't like the colours

# Define colors for each class (in order of your ID list)
land_cover_colors <- c(
  # Cropland (browns/yellows)
  "#D2691E",  # Herbaceous rainfed cropland - saddle brown
  "#CD853F",  # Tree/shrub rainfed cropland - peru
  "#DAA520",  # Irrigated cropland - goldenrod
  
  # Forests (greens - darker for closed, lighter for open)
  "#006400",  # Closed evergreen broadleaved - dark green
  "#228B22",  # Open evergreen broadleaved - forest green
  "#2E8B57",  # Closed deciduous broadleaved - sea green
  "#3CB371",  # Open deciduous broadleaved - medium sea green
  "#004d25",  # Closed evergreen needleleaved - very dark green
  "#0d5f32",  # Open evergreen needleleaved - dark forest green
  "#1e6b3a",  # Closed deciduous needleleaved - dark green
  "#2e7d47",  # Open deciduous needleleaved - medium green
  "#0F4C0A",  # Closed mixed-leaved - very dark forest green
  "#4d7c47",  # Open mixed-leaved - olive green
  
  # Shrublands (olive/yellow-greens)
  "#808000",  # Evergreen shrubland - olive
  "#9ACD32",  # Deciduous shrubland - yellow green
  
  # Grassland
  "#7CFC00",  # Grassland - lawn green
  
  # Lichens and mosses
  "#98FB98",  # Light green
  
  # Wetlands (blues/teals)
  "#4682B4",  # Swamp - steel blue
  "#5F9EA0",  # Marsh - cadet blue
  "#87CEEB",  # Lake-river flat - sky blue
  "#B0C4DE",  # Saline - light steel blue
  "#20B2AA",  # Mangrove forest - light sea green
  "#48D1CC",  # Salt marsh - medium turquoise
  "#AFEEEE",  # Tidal flat - pale turquoise
  
  # Urban (grays)
  "#696969",  # Urban impervious - dim gray
  "#A9A9A9",  # Rural impervious - dark gray
  
  # Sparse vegetation
  "#F5DEB3",  # Wheat
  
  # Bare areas
  "#D2B48C",  # Tan
  
  # Water
  "#0000FF",  # Blue
  
  # Ice/snow
  "#FFFAFA"   # Snow white
)

# Create named vector matching your data
names(land_cover_colors) <- levels_germany_masked$class

# Apply to your plot
par(mar=c(6,4,4,6))
plot(germany_masked, col = land_cover_colors, legend = FALSE, 
     axes = FALSE, box = FALSE)
axis(1, line = 4)
axis(2)





#change bounding box

# Plot without default axes and box
par(mar=c(6,4,4,2))
plot(germany_masked, col = rev(viridis(n_classes)), legend = FALSE, 
     axes = FALSE, box = FALSE)

# Add only bottom and left axes
axis(1, line =4)  # bottom axis (x-axis)
axis(2)  # left axis (y-axis)

# Add legend
legend("topright", legend = levels_germany_masked$class, 
       fill = rev(viridis(n_classes)), cex = 0.7, bty = "n")

# 
# # change position of legend
# 
# usr <- par("usr")
# legend(x = usr[2] + (usr[2] - usr[1]) * 0.05, y = usr[4], 
#        legend = levels_germany_masked$class, 
#        fill = rev(viridis(n_classes)), cex = 0.7, bty = "n", xpd = TRUE)

# make map interactive

germany_masked_fact <- as.factor(germany_masked)

levels(germany_masked_fact) <- list(levels_germany_masked)

pal <- colorFactor(palette = viridis(nrow(levels_germany_masked)),
                   domain = levels_germany_masked$ID)

memory.limit(size = 16000) 

germany_map <- leaflet() %>%
  addTiles() %>%
  addRasterImage(germany_masked_fact, colors = pal, opacity = 0.8) %>%
  addLegend("topright", pal = pal, values = levels_germany_masked$ID,
            labels = levels_germany_masked$class,
            title = "Land Use Classes")

# too large

# plot(germany_masked, col = rev(viridis(n_classes)), legend = FALSE)
# current_limits <- par("usr")  # gets current plot limits
# 
# # Then create a new plot with expanded limits
# buffer <- 50000  # adjust as needed
# xlim_expanded <- c(current_limits[1] - buffer, current_limits[2] + buffer)
# ylim_expanded <- c(current_limits[3] - buffer, current_limits[4] + buffer)
# 
# # Replot with expanded limits
# plot(germany_masked, col = rev(viridis(n_classes)), legend = FALSE,
#      xlim = xlim_expanded, ylim = ylim_expanded)
# 
# legend("topright", legend = levels_germany_masked$class, 
#        fill = rev(viridis(n_classes)), cex = 0.7, bty = "n")



# try only with Hessia as not enough RAM

# Download GADM Level 1 data for Germany (includes Bundesländer)
germany_states <- gadm("Germany", level = 1, path = "data/")

# Check available states
unique(germany_states$NAME_1)

hessen <- germany_states[germany_states$NAME_1 == "Hessen", ]

hessen_only <- crop(germany_masked, hessen)
hessen_only_masked <- mask(hessen_only, hessen)

writeRaster(hessen_only_masked, "hessen_only_masked.tif", overwrite =T)

plot(hessen_only_masked)

levels_hessen_only_masked <- levels(hessen_only_masked)
print(levels_hessen_only_masked)

# reverse colour palette so that water is blue
pal <- colorFactor(palette = rev(viridis::viridis(nrow(levels_hessen_only_masked[[1]] ))),
                   domain = levels_hessen_only_masked$ID)


# make interactive map with leaflet
hessen_map <- leaflet() %>%
  addTiles() %>%
  addRasterImage(hessen_only_masked, colors = pal, opacity = 0.8) %>%
  addLegend("topright", pal = pal, values = levels_hessen_only_masked$ID,
            labels = levels_germany_masked$class,
            title = "Land Use Classes")

# safe map
saveWidget(hessen_map, "hessen_map_glc_fc10.html", selfcontained = TRUE)
# too large



# trying mapview
mapview(hessen_only_masked, col.regions = pal)
#"only" shows 500.000 Pixel
# increase up to recommended 690730524

mapview(hessen_only_masked, col.regions = pal, maxpixels = 690730524)

# way too large

# try to reduce resolution (downsample)

# Reduce resolution by a factor of 10
hessen_agg <- aggregate(hessen_only_masked, fact = 10, fun = "modal")  # 'modal' for categorical data, ensures the most frequent category is used in each block

writeRaster(hessen_agg, "hessen_agg.tif", overwrite =T)

levels(hessen_agg) <- levels(hessen_only_masked)

# check unique values in hessen_agg

unique_values <- unique(values(hessen_agg))
sort(na.omit(unique_values))

present_ids <- sort(na.omit(unique(values(hessen_agg))))
levels_present <- levels_hessen_only_masked[[1]][levels_hessen_only_masked[[1]]$ID %in% present_ids, ]

pal <- colorFactor(palette = rev(viridis::viridis(nrow(levels_present ))),
                   domain = levels_present$ID)

levels(hessen_agg) <- list(levels_present)
str(hessen_agg)

hessen_agg_fact <- as.factor(hessen_agg)
levels(hessen_agg_fact) <- list(levels_present)

str(levels(hessen_agg_fact))

setdiff(unique(values(hessen_agg_fact)), levels_present$ID)

# remove NA

raster_values <- unique(na.omit(values(hessen_agg_fact)))
# missing_ids <- setdiff(raster_values, levels_present$ID)
# print(missing_ids)

levels_present <- levels_hessen_only_masked[[1]][levels_hessen_only_masked[[1]]$ID %in% raster_values, ]
num_classes <- nrow(levels_present)

pal <- colorFactor(
  palette = rev(viridis(num_classes)),
  domain = levels_present$ID
)


# Check if levels_present is empty
if(nrow(levels_present) == 0) {
  print("levels_present is empty! Using original levels...")
  levels_present <- levels_hessen_only_masked[[1]]
}

# Force data type alignment
actual_values_char <- as.character(actual_values)
levels_present$ID <- as.character(levels_present$ID)

# Try the match again
levels_present_filtered <- levels_present[levels_present$ID %in% actual_values_char, ]

num_classes <- nrow(levels_present)
pal <- colorFactor(
  palette = rev(viridis(num_classes)),
  domain = levels_present$ID,
  na.color = "transparent"
)

# Create the map
mapview(hessen_agg_fact, col.regions = pal, maxpixels = 6909617)
