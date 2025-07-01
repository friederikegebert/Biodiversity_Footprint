library(sf)

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


str(birds_sf)
names(birds_sf)
head(birds_sf)
unique(birds_sf$sci_name)

plot(birds_sf["sci_name"])

crop_to_germany <- function(data){ 
germany_sf <- ne_countries(country = "Germany", scale = "medium", returnclass = "sf")
germany_sf <- st_transform(germany_sf, crs = crs(data))
germany_vect <- vect(germany_sf)
r_crop <- crop(data, germany_vect)
germ <- mask(r_crop, germany_vect)
plot(germ)
return(germ)
}
