
# ------------------------------------------------------------------------- #
# Centro de Investigación Estadística ERGOSTATS #
# Autor: Alex Bajaña #
# Fecha: 2025-01-20 #
# ------------------------------------------------------------------------- #


# Cargamos las librerias --------------------------------------------------

library(tidyverse)
library(haven)
# Para manejo de encuestas
library(survey)
library(srvyr)
library(sf)
# Para graficos
library(cowplot)

# Cargamos los datos ------------------------------------------------------

carpetas <- list.dirs("data/") |> 
  str_subset("ESPAC_")



archivos <- list.files("data/ESPAC 2023/", full.names = T,pattern = "sav$")

# Mar la creacion de archivos con el elementos carpetas:

archivos <- map(carpetas, ~list.files(.x, full.names = T,pattern = "sav$")) |> 
  map(str_subset, pattern = "eunac|sunac") 

espac_list <- archivos |> 
  map(function(archivo) map(archivo,read_sav))

espac_list <- espac_list |> 
  map2(.y = c(2019:2021),
       function(tabla, anio) map(tabla,mutate, anio = anio))


map(espac_list, ~attributes(.x))

# metadatos de archivo sav:

etiquetas <- map(.x = espac_list[[1]],
    .f = function(tabla)map(tabla,attributes)) |> 
  map(.f = function(lista)map(lista,"label")) |> 
  # unlist
  map(.f = unlist) 

# Uso de suelo 16
# Empleo la 8
# 1. Uso de suelo 


datos_uso <- espac_list |> 
  map(2) |> 
  map(rename_with, str_to_lower) |> 
  map(select,identificador:ual_segm,starts_with("su_|al_"),su_k202ha,su_categoria,fact_exp_fin, anio)

# 2. Empleo

# datos_empleo <- read_sav(archivos[8]) |> 
#   as_tibble()

# map(datos_empleo, ~attributes(.x)$"label")

datos_empleo <- espac_list |>
  map(1) |> 
  map(rename_with, str_to_lower) |> 
  map(select,identificador:ual_segm,starts_with("al_|"),eu_superficie_ha,eu_k1301,fact_exp_fin, anio)


# 3. Precipitacion

datos_precipitacion <- list.files("data/precipitacion/", full.names = T) |> 
  map(read_csv) |> 
  map(rename_with, str_to_lower) |> 
  # Asignar año con map 2 usando el venctor 2019, 2020 y 2021
  map2(.y = c(2019:2021), ~mutate(.x, anio = .y)) |> 
  reduce(bind_rows) 



# 4. Shapres

shp_ecuador <- st_read("data/SHAPES/nxprovincias.shp") |> 
  rename_with(str_to_lower)


# Procesamiento adicional -------------------------------------------------

# 1. Unimos el empleo
datos_empleo <- datos_empleo |> 
  reduce(bind_rows) |>
  select(
    identificador = identificador,
    provincia = ual_prov,
    estrato = ual_estr,
    segmento = ual_segm,
    empleo = eu_k1301,
    superficie = eu_superficie_ha,
    factor_expansion = fact_exp_fin,
    anio = anio
  )

# 2. Unimos todos los anios y renombramos las variables
datos_uso <- datos_uso |> 
  map(mutate, su_categoria = haven::as_factor(su_categoria)) |>
  map(mutate, su_categoria = forcats::fct_relabel(su_categoria,.fun = str_trim, side = "both")) |>
  reduce(bind_rows) |>
  select(
    identificador = identificador,
    provincia = ual_prov,
    estrato = ual_estr,
    segmento = ual_segm,
    uso_suelo = su_categoria,
    superficie = su_k202ha,
    factor_expansion = fact_exp_fin,
    anio = anio
  ) 


# 3. Procesar el shape y crear un diccionario de provincia

etiqueta_prov <- shp_ecuador |> 
  select(dpa_provin, dpa_despro) |> 
  as_tibble()
  
shp_ecuador <- shp_ecuador |> 
  select(-dpa_anio, -pee_codigo,-dpa_valor,-rei_codigo,-ren_codigo)


# Estadisticas principales ---------------------------------------------------

shape_precipitacion <-    shp_ecuador|>
  left_join(datos_precipitacion |> 
               select(-dpa_despro)) |>
  filter(dpa_provin != 20)


plot_1_precipitacion <- shape_precipitacion |> 
  ggplot(aes(fill = mean)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_minimal() +
  # Para los textos vamos a ocupar un 
  theme(legend.position = "bottom") +
  facet_wrap(.~anio) +
  labs(
    title = "Precipitación en Ecuador",
    subtitle = "Promedio anual de precipitación en mm",
    fill = "Precipitación (mm)"
  )

ggsave("presentaciones/c2_01_precipitacion_anual.png", plot_1_precipitacion, width = 20, height = 10)



# 2. Diferencia lag por anio de la precipitacion 
# Usamos group by y lag de dplyr 

shape_precipitacion_diff <- shape_precipitacion |> 
  group_by(dpa_provin) |>
  arrange(anio) |>
  mutate(lag_precipitacion = mean - dplyr::lag(mean)) |> 
  ungroup() |> 
  filter(!is.na(lag_precipitacion)) 

plot_2_precipitacion <- shape_precipitacion_diff |>
  ggplot(aes(fill = lag_precipitacion)) +
  geom_sf() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(.~anio) +
  labs(
    title = "Diferencia de precipitación en Ecuador",
    subtitle = "Diferencia de precipitación en mm",
    fill = "Diferencia de precipitación (mm)"
  )

ggsave("presentaciones/c2_02_diferencia_precipitacion_anual.png", plot_2_precipitacion, width = 15, height = 10)


datos_uso |> 
  filter(uso_suelo %in% c("PERMANENTES","TRANSITORIOS","PERMANENTE O PERENNE","TRANSITORIO")) |> 
  group_by(identificador, provincia, anio) |>
  summarise(superficie = sum(superficie), .groups = "drop") 

