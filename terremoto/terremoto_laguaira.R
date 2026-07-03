# 1. Cargar librerías necesarias
{
  library("rvest")
  library('sf')
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
  # source("points.R") # Descomenta si es estrictamente necesario
}

# Limpiar variables conflictivas previas
rm(list = ls(pattern = "puntos|coordenadas"))

# 2. Configuración de Estilos y Fuentes
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
st <- glue("{title}<span style='color:#197176;'></span>")
cap <- c("")

# 3. Cargar y procesar Datos Geográficos
map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1) %>% 
  st_as_sf()

# Dataframe base de los sismos (Mantenemos el formato plano para las ondas estables)
puntos_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05"),
  lat = c(10.43, 10.401,10.48),    
  long = c(-68.62, -68.321, -66.90)  
)

# Convertir a objeto espacial sf únicamente para la base del mapa y las etiquetas
puntos_sf <- st_as_sf(puntos_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf <- st_transform(puntos_sf, crs = st_crs(dataNew))

# 4. Construcción del Mapa Final
A <- ggplot() +
  # Capa Base 1: Fondo de los municipios coloreados
  geom_sf(data = dataNew, aes(fill = N), show.legend = FALSE, size = 0.05) +
  
  # Capa Base 2: Máscara oscura y estilizada con bordes blancos para los municipios
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.05) +
  
  # SOLUCIÓN CRÍTICA: Dibujar las ondas expansivas concéntricas usando coord_sf heredado
  # Esto evita que geom_sf() colapse por tamaños (size) gigantescos incompatibles.
  geom_point(data = puntos_terremoto, aes(x = long, y = lat), color = "red", size = 45, alpha = 0.08) + 
  geom_point(data = puntos_terremoto, aes(x = long, y = lat), color = "red", size = 22, alpha = 0.15) + 
  geom_point(data = puntos_terremoto, aes(x = long, y = lat), color = "red", size = 2.0, alpha = 1) + 
  
  # Capa 4: Etiquetas repelentes optimizadas
  geom_label_repel(
    data = puntos_sf,
    aes(label = nombre, geometry = geometry), # Pasamos explícitamente la geometría asignada
    stat = "sf_coordinates",       
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.4,           
    
    box.padding = 0.6,            
    point.padding = 0.2,          
    label.padding = 0.18,         
    force = 2.0,                  
    nudge_y = c(1.5, 0.8),         # Ajustado levemente hacia arriba para mejor perspectiva
    direction = "both",           
    
    color = "black",              
    fill = "white",               
    fontface = "bold",            
    size = 2.5,                   
    max.overlaps = Inf            
  ) +
  
  # Sistema de coordenadas espacial obligatorio (Une las capas sf con los puntos planos)
  coord_sf(crs = 4326, clip = "off") +
  
  # Mapear títulos
  labs(
    x = NULL, y = NULL,
    caption = cap,
    title = st
  ) +
  
  # Configuración temática del mapa
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(5, 30, 5, 5),
    
    # CENTRADO COMPLETO DEL TÍTULO
    plot.title.position = "panel",   
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,                   
      halign = 0.5,                  
      vjust = 1,
      margin = margin(b = 15, t = 10),
      family = body_font,
      maxwidth = 0.9                 
    ),
    
    plot.caption.position = "plot",
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    
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

# 5. Renderizar mapa
A
