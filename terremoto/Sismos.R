{
library("rvest")
library('sf')
library("ggplot2")
library("tmap")
library("unikn")
library("dplyr")
library("ggrepel")
library("leaflet")
library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(ggview)
library(ggh4x)
library(cowplot)
library(grid)
  source("points.R")
}

rm(list = ls(pattern = "puntos|coordenadas"))

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"
font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"

title <- glue("<span style='font-family:{title_font};font-size:15pt;'>**Terremotos en Venezuela [06-24-2026]**</span><br>")
st <- glue("{title}</span><span style='color:#197176;'></span>")
cap <- c("")


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
    nudge_y = c(0.9, 0.5),        # Empuja los textos hacia el norte
    direction = "both",           
    
    color = "black",              # Color de la fuente
    fill = "white",               # Fondo del cuadro de texto para legibilidad
    fontface = "bold",            # Texto en negrita
    size = 2.2,                   # Tamaño de letra equilibrado y estético
    max.overlaps = Inf            # Asegura que ambas etiquetas se rendericen siempre
  ) +
  
  # OBLIGATORIO: Forzar el sistema de coordenadas espacial y evitar recortes
  coord_sf(clip = "off") +
  
  # Ajustes tipográficos, títulos y remoción de ejes innecesarios
  labs(
    x = NULL, y = NULL,
    caption = cap,
    tag = st
  ) +
  # Usamos theme_minimal pero limpiamos explícitamente lo que no queremos de él
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    # Eliminamos las grillas y textos de los ejes que activa theme_minimal
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(5, 30, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.tag.position = c(0.01, 0.99),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      vjust = 1,
      valign = 1,
      margin = margin(b = 5, t = 0),
      family = body_font,
      maxwidth = 0.63
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

# 5. Renderizar y mostrar el mapa definitivo
A

