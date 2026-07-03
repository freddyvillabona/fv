{
library("rvest")
library('sf')
library("ggplot2")
library("tmap")
library("unikn")
library("dplyr")
library("ggrepel")
  source("points.R")
}
# 1. Cargar mapa base y configurar datos
map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1) %>% 
  st_as_sf()

# 2. Generar el objeto de puntos unificado (Mismo día: 24 de Junio)
puntos_terremoto <- data.frame(
  nombre = c("Precursor (Mw 7.2) 18:04", "Principal (Mw 7.5) 18:05"),
  lat = c(10.43, 10.401),    
  long = c(-68.62, -68.321)  
)

# Convertir a objeto espacial y homologar la proyección del mapa base
puntos_sf <- st_as_sf(puntos_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf <- st_transform(puntos_sf, crs = st_crs(dataNew))

# Extraer coordenadas planas explícitas para evitar errores en ggrepel
coordenadas <- as.data.frame(st_coordinates(puntos_sf))
puntos_sf$X <- coordenadas$X
puntos_sf$Y <- coordenadas$Y

# 3. Graficar Mapa A corregido
A <- ggplot() +
  # Capa Base 1: Fondo coloreado por N
  geom_sf(data = dataNew, aes(fill=N), show.legend = TRUE, size = 0.05) +
  
  # Capa Base 2: Máscara negra semitransparente con bordes blancos
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.05) +
  
  # Capa 3: Puntos de los epicentros (Usamos el mismo objeto puntos_sf)
  geom_sf(data = puntos_sf, color = "red", size = 1, alpha = 0.9) + 
  
  # Capa 4: Etiquetas repelentes vinculadas exactamente a los mismos puntos
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre),
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.5,           # Líneas más estilizadas (1.6 era muy grueso)
    box.padding = 1.2,            # Mayor separación para que no se encima el texto
    point.padding = 0.3,
    color = "black",              
    fill = "white",               
    fontface = "bold",
    size = 1.5,                   # Tamaño aumentado a 3.5 para que sea legible
    max.overlaps = Inf            # Obliga a mostrar ambas etiquetas siempre
  ) +
  
  # Ajustes de diseño y títulos
  ggtitle("TERREMOTOS EN VENEZUELA") +
  xlab("") + ylab("") +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 20)) +
  labs(fill = "Відсоток")

# Renderizar mapa
A
