{
  library("rvest")
  library("sf")
  library("ggplot2")
  library("tmap")
  library("unikn")
  library("dplyr")
  library("ggrepel")
  library("leaflet")
  library("tidyverse")
  library("showtext")
  library("ggtext")
  library("glue")
  library("ggview")
  library("ggh4x")
  library("cowplot")
  library("grid")

}
# Limpieza de memoria para evitar conflictos
rm(list = ls(pattern = "puntos|coordenadas"))

# Configuración de colores y estilos visuales
bg_col <- "#FFFFFF"
text_col <- "#151C28"
highlight_col <- "#7F055F"

# Configuración de tipografías desde Google Fonts
font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font = "Nunito"

# Definimos el título dinámico con formato ggtext

cap <- c("")

title_text <- glue("<span style='font-family:{title_font};font-size:15pt;'>**Doblete sísmico en Venezuela [06-24-2026]<br>**</span>")

# Lectura del mapa base de los municipios de Venezuela
map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1) %>% 
  st_as_sf()

# Dataframe 1: Puntos base y etiquetas principales
puntos_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
)

puntos2_terremoto <- data.frame(
  nombre = c("Precursor (Mw 7.2) 18:04:33", "Principal (Mw 7.5) 18:05:12", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
)

puntos3_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
)

# Conversión de coordenadas geográficas a objetos espaciales (sf)
puntos_sf <- st_as_sf(puntos_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf2 <- st_as_sf(puntos2_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf3 <- st_as_sf(puntos3_terremoto, coords = c("long", "lat"), crs = 4326)

# Homologar la proyección espacial de los puntos con la del mapa base
puntos_sf <- st_transform(puntos_sf, crs = st_crs(dataNew))
puntos_sf2 <- st_transform(puntos_sf2, crs = st_crs(dataNew))
puntos_sf3 <- st_transform(puntos_sf3, crs = st_crs(dataNew))

# Extraer las coordenadas numéricas proyectadas reales
coordenadas <- as.data.frame(st_coordinates(puntos_sf))
puntos_sf$X <- coordenadas$X
puntos_sf$Y <- coordenadas$Y

# SOLUCIÓN COMPLETA: Calculamos vectores manuales proporcionales a las dimensiones del mapa proyectado
# Esto expande las etiquetas en abanico evitando que colisionen en el centro del evento
puntos_sf <- puntos_sf %>%
  mutate(
    empuje_x = c(-180000, -80000,  180000,   0), # Ajuste manual en el eje X según el punto
    empuje_y = c( 120000, -120000, -100000, 150000)  # Ajuste manual en el eje Y según el punto
  )