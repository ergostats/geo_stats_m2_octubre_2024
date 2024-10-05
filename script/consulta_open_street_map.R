library(osmdata)
library(tidyverse)
library(sf)
library(readxl)
# Pimero pedimos una boundary box



box <- getbb(place_name = "Quito, Ecuador") 

# Cargar y preparar los datos
zonas <- st_read("data/GEODATABASE_NACIONAL_2021/GEODATABASE_NACIONAL_2021.gdb/", layer = "zon_a")

dmq_zonas <- zonas %>% filter(str_detect(zon, "^170150"))


dmq_zonas %>% 
  ggplot() +
  geom_sf()

# Plantas de energia
osm_query <- opq(box) %>%
  add_osm_feature(value = "plant", key = "power") %>% 
  osmdata_sf()


# Los mapas vienen con un sistema de refencia de coordenadas

st_crs(dmq_zonas)

st_crs(osm_query$osm_points)

puntos <- osm_query$osm_points

puntos <- puntos %>% 
  st_transform(crs = st_crs(dmq_zonas))

dmq_zonas %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = puntos)



## Buscar una direccion

amazonas <- opq(box) %>% 
  add_osm_feature(key = "addr:street", value = "6 de Diciembre") %>% 
  osmdata_sf()



dmq_zonas %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = amazonas$osm_points)

# Como puedo mandar varias

direcciones <- read_excel("data/DIRECCIONES UCL-MoE.xlsx",
                          sheet = "DISTRITOS A NIVEL NACIONAL", skip = 5)

direcciones <- direcciones %>% 
  filter(str_detect(`NUMERO DE DISTRITO`,"ZONA",negate = T) )


pichincha <- direcciones %>% 
  filter(PROVINCIA == "PICHINCHA")



box <- getbb(place_name = "Pichincha, Ecuador") 

opq(bbox = box) %>% 
  add_osm_feature(key = "addr:street", "Avenida Marquesa de Solanda") %>%
  # add_osm_feature(key = "addr:street", "Victor Velazco") %>%
  osmdata_sf()
