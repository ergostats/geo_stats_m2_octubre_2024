library(sf)
library(tidyverse)
library(leaflet)

leaflet::tile

leaflet() %>%
  setView(lng = -78.1834, lat = -1.8312, zoom = 7) %>% 
  addWMSTiles(
    "http://geoportal.agricultura.gob.ec/agroestadistica/riesgos_agroclimaticos/wms?service=wms&version=1.3.0",
    layers = "GetCapabilities",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )


# Instalar y cargar paquetes
install.packages(c("httr", "XML", "leaflet"))
library(httr)
library(XML)
library(leaflet)

# URL del GetCapabilities
wms_url <- "http://geoportal.agricultura.gob.ec/agroestadistica/riesgos_agroclimaticos/wms?service=wms&version=1.3.0&request=GetCapabilities"

# Obtener las capas disponibles
respuesta <- GET(wms_url)
if (status_code(respuesta) == 200) {
  contenido <- content(respuesta, as = "text", encoding = "UTF-8")
  xml_parsed <- xmlParse(contenido)
  capas_nodos <- getNodeSet(xml_parsed, "Layer")
  nombres_capas <- sapply(capas_nodos, xmlValue)
  print("Capas disponibles:")
  print(nombres_capas)
} else {
  cat("Error en la solicitud:", status_code(respuesta))
}

# Seleccionar la capa a visualizar
capa_seleccionada <- "riesgos_agroclimaticos:riesgos_agroclimaticos_niveles_multiriesgo"  # Reemplaza con la capa deseada

# Crear el mapa y agregar la capa WMS
lat_ecuador <- -1.8312
lng_ecuador <- -78.1834

mapa <- leaflet() %>%
  addTiles() %>%
  setView(lng = lng_ecuador, lat = lat_ecuador, zoom = 7) %>%
  addWMSTiles(
    baseUrl = "http://geoportal.agricultura.gob.ec/agroestadistica/riesgos_agroclimaticos/wms",
    layers = capa_seleccionada,
    options = WMSTileOptions(
      format = "image/png",
      version = "1.3.0"
    ),
    attribution = "© Ministerio de Agricultura y Ganadería de Ecuador"
  )

# Mostrar el mapa
mapa

