#+++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# IUCN data
#
# Author: Friederike Gebert
# 
# date: 08/07/2025
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readr)
library(dplyr)
library(ggplot2)
library(sf)       
library(leaflet)   

# Example: Trees, Fagales to Lamiales

Fag_Lam <- read.csv("TREES_Magnoliopsida_Fagales_to_Lamiales.csv")
names(Fag_Lam)
saveRDS(Fag_Lam, file="Fag_Lam.rds")


str(Fag_Lam$dec_lat)
str(Fag_Lam$dec_long)

# characters -> convert to numeric

Fag_Lam <- Fag_Lam %>%
  mutate(
    dec_lat = as.numeric(dec_lat),
    dec_long = as.numeric(dec_long)
  ) %>%
  filter(!is.na(dec_lat) & !is.na(dec_long))


# plot on world map
world <- map_data("world")


ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_point(data = Fag_Lam, aes(x = dec_long, y = dec_lat),
             color = "deeppink", size = 2, alpha = 0.7) +
  coord_fixed(1.3) +
  theme_minimal()

# interactive map

leaflet(data = Fag_Lam) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~dec_long,
    lat = ~dec_lat,
    popup = ~sci_name,
    radius = 1,
    fillOpacity = 0.8,
    color = "deeppink"
  )
