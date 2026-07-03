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

# Limpieza de memoria para evitar conflictos
rm(list = ls(pattern = "puntos|coordenadas"))

# Configuración de colores y estilos visuales
bg_col <- "#CCCCCC"
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
title_text <- glue("<span style='font-family:{title_font};font-size:15pt;'>**Doblete sísmico en Venezuela [06-24-2026]<br>**</span>")
cap <- c("")

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

A <- ggplot() +
  # Capa Base 1: Fondo de los municipios coloreados
  geom_sf(data = dataNew, aes(fill = N), show.legend = FALSE, size = 0.05) +
  
  # Capa Base 2: Máscara oscura y estilizada con bordes blancos para los municipios
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.05) +
  
  # Capa 3: Puntos de los epicentros con tamaño proporcional y visible
  geom_sf(data = puntos_sf3, color = "#FFFF00", size = 60.5, alpha = 0.1) + 
  geom_sf(data = puntos_sf2, color = "red", size = 30.5, alpha = 0.2) + 
  geom_sf(data = puntos_sf, color = "#5f0404", size = 0.5, alpha = .8) + 
  
  # Capa 4: SOLUCIONADO - Etiquetas repelentes con empuje vectorial adaptado a metros
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre),
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.4,           
    
    box.padding = 0.5,            
    point.padding = 0.2,          
    label.padding = 0.2,         
    force = 50.0,                 # Forzar el reposicionamiento final
    
    # Mapeo de los vectores adaptados a la proyección métrica
    nudge_x = puntos_sf$empuje_x,
    nudge_y = puntos_sf$empuje_y,
    direction = "both",           
    
    color = "black",              
    fill = "white",               
    fontface = "bold",            
    size = 2.2,                   
    max.overlaps = Inf            
  ) +
  
  # Evitar recortes de las cajas de texto en los bordes del mapa
  coord_sf(clip = "off") +
  
  # Estructura de títulos
  labs(
    x = NULL, y = NULL,
    title = title_text,
    caption = cap
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(15, 15, 15, 15), 
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      valign = 1,
      margin = margin(b = 15, t = 0), 
      family = body_font,
      maxwidth = 0.95 
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    strip.text = element_textbox_simple(
      face = "bold",
      margin = margin(t = 10),
      size = rel(0.8)
    )
  ) 

# Mostrar mapa final sin colisiones
B <- ggdraw(A) +
  draw_text(
    x = 0.2, y = 0.2,
    size = 11,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("", 17)
  )

B
