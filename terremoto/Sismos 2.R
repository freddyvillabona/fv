
{
library("rvest")
library('sf')
library("ggplot2")
library("tmap")
library("unikn")
library("dplyr")
library("ggrepel")
library("leaflet")
}

rm(list = ls(pattern = "puntos|coordenadas"))

map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1) %>% 
  st_as_sf()

puntos_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05"),
  lat = c(10.43, 10.401),    
  long = c(-68.62, -68.321)  
)

puntos2_terremoto <- data.frame(
  nombre = c("Precursor (Mw 7.2) 18:04", "Principal (Mw 7.5) 18:05"),
  lat = c(10.43, 10.401),    
  long = c(-68.62, -68.321)  
)

puntos3_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05"),
  lat = c(10.43, 10.401),    
  long = c(-68.62, -68.321)  
)

puntos_sf <- st_as_sf(puntos_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf2 <- st_as_sf(puntos2_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf3 <- st_as_sf(puntos3_terremoto, coords = c("long", "lat"), crs = 4326)

# Homologar la proyección espacial de los puntos con la del mapa base
puntos_sf <- st_transform(puntos_sf, crs = st_crs(dataNew))
puntos_sf2 <- st_transform(puntos_sf2, crs = st_crs(dataNew))
puntos_sf3 <- st_transform(puntos_sf3, crs = st_crs(dataNew))

# Extraer las coordenadas proyectadas en ejes X e Y para el algoritmo ggrepel
coordenadas <- as.data.frame(st_coordinates(puntos_sf))
puntos_sf$X <- coordenadas$X
puntos_sf$Y <- coordenadas$Y

# 4. Construcción del Mapa Final A
A <- ggplot() +
  # Capa Base 1: Fondo de los municipios coloreados
  geom_sf(data = dataNew, aes(fill = N), show.legend = FALSE, size = 0.05) +
  
  # Capa Base 2: Máscara oscura y estilizada con bordes blancos para los municipios
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.05) +
  
  # Capa 3: Puntos de los epicentros con tamaño proporcional y visible
  geom_sf(data = puntos_sf3, color = "red", size = 60.5, alpha = 0.1) + 
  geom_sf(data = puntos_sf2, color = "red", size = 30.5, alpha = 0.2) + 
  geom_sf(data = puntos_sf, color = "red", size = 1.5, alpha = 1) + 


  
  # Capa 4: Etiquetas repelentes configuradas para despejar el territorio nacional
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre),
    min.segment.length = 0,       # Fuerza a que siempre se dibuje la línea roja
    segment.color = "red",        # Color de la línea conectora
    segment.size = 0.4,           # Grosor estilizado de la línea
    
    # Ajustes de posición para alargar líneas y enviar el texto al mar Caribe
    box.padding = 0.6,            # Espacio libre para permitir el estiramiento
    point.padding = 0.2,          # Distancia de inicio respecto al punto rojo
    label.padding = 0.18,         # Margen interno para hacer las cajas de texto compactas
    force = 2.0,                  # Incrementa la repulsión para alejar el texto del mapa
    nudge_y = c(0.9, 0.5),        # Empuja los textos hacia el norte (Yaracuy más arriba que Carabobo)
    direction = "both",           
    
    color = "black",              # Color de la fuente
    fill = "white",               # Fondo del cuadro de texto para legibilidad
    fontface = "bold",            # Texto en negrita
    size = 2.2,                   # Tamaño de letra equilibrado y estético
    max.overlaps = Inf            # Asegura que ambas etiquetas se rendericen siempre
  ) +
  
  # Ajustes tipográficos, títulos y remoción de ejes innecesarios
  ggtitle("TERREMOTOS EN VENEZUELA") +
  xlab("") + ylab("") +
  theme_void() +                  # Elimina la cuadrícula de fondo, latitudes y longitudes
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.1),
    plot.subtitle = element_text(size = 20)
  )

# 5. Renderizar y mostrar el mapa definitivo
A

