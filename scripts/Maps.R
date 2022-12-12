rm(list = ls())
setwd(r'(C:\Users\juan.velasquez\OneDrive - Universidad de los Andes\Maestria\Semestres\2022-2\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\Trabajo\Predicting-dengue-spread\scripts)')
#### **Instalar/llamar las librerías de la clase**
require(pacman)
p_load(
  tidyverse, rio, skimr, viridis, osmdata,
  ggsn, ## scale bar
  raster, stars, ## datos raster
  ggmap, ## get_stamenmap
  sf, ## Leer/escribir/manipular datos espaciales
  leaflet
)
## download boundary
## get San Juan Puerto RICO
sj <- opq(bbox = getbb("San Juan, Puerto Rico, Estados Unidos de América")) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()
sj <- sj$osm_multipolygons %>% subset(admin_level == 6)
## get Medallo- Comunas
iq <- opq(bbox = getbb("Iquitos, Maynas, Loreto, Perú")) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()
iq <- iq$osm_multipolygons %>% subset(admin_level == 8)

### plot San Juan}
sj$ramdon <- runif(nrow(sj), 10, 20)

map_sj <- ggplot() +
  geom_sf(data = sj, aes(fill = ramdon))+
  scale_fill_viridis(option = "D", name = "light activity")
map_sj
## add Scale_bar
map_sj <- map_sj + north(data = sj, location = "topleft", symbol = 1) +
  scalebar(data = sj, dist = 5, transform = T, dist_unit = "km")
## add theme
map_sj <- map_sj + theme_linedraw()
## add osm layer
osm_layer_sj <- get_stamenmap(
  bbox = as.vector(st_bbox(sj)),
  maptype = "toner", source = "osm", zoom = 13
)
map2_sj <- ggmap(osm_layer_sj) +
  geom_sf(data = sj, aes(fill = ramdon), alpha = 0.3, inherit.aes = F) +
  scale_fill_viridis(option = "E", name = "light activity") +
  scalebar(data = sj, dist = 5, transform = T, dist_unit = "km") +
  north(data = sj, location = "topleft") + theme_linedraw() + labs(x = "", y = "")
map2_sj




### plot Iquitos
iq$ramdon <- runif(nrow(sj), 10, 20)

map_iq <- ggplot() +
  geom_sf(data = iq, aes(fill = ramdon))+
  scale_fill_viridis(option = "D", name = "light activity")
map_iq
## add Scale_bar
map_iq <- map_iq + north(data = iq, location = "topleft", symbol = 1) +
  scalebar(data = siq, dist = 5, transform = T, dist_unit = "km")
## add theme
map_iq <- map_iq + theme_linedraw()
## add osm layer
osm_layer_iq <- get_stamenmap(
  bbox = as.vector(st_bbox(iq)),
  maptype = "toner", source = "osm", zoom = 13
)
map2_iq <- ggmap(osm_layer_iq) +
  geom_sf(data = iq, aes(fill = ramdon), alpha = 0.3, inherit.aes = F) +
  scale_fill_viridis(option = "E", name = "light activity") +
  scalebar(data = iq, dist = 5, transform = T, dist_unit = "km") +
  north(data = iq, location = "topleft") + theme_linedraw() + labs(x = "", y = "")
map2_sj

