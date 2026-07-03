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
  library("ggplot2")
  library("dplyr")
  library("hrbrthemes")
  library("viridis")
  library("readxl")
  library("patchwork") 
  library("viridisLite")
  library("magick")
  source("points.R")
}

rm(list = ls(pattern = "puntos|coordenadas"))

bg_col <- "#CCCCCC"
text_col <- "#151C28"
highlight_col <- "#7F055F"
font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"

# Definimos el título limpio con ggtext
title_text <- glue("<span style='font-family:{title_font};font-size:15pt;'>**Doblete sísmico en Venezuela [06-24-2026]<br>**</span>")
cap <- c("")

c1 <- glue("<span style='font-family:{title_font};font-size:8pt;'><br>**</span>")

map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1) %>% 
  st_as_sf()

# Dataframe 1: Puntos base y etiquetas (4 puntos)
puntos_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
)

# Dataframe 2: Segunda onda concéntrica (4 puntos)
puntos2_terremoto <- data.frame(
  nombre = c("Precursor (Mw 7.2) 18:04:33", "Principal (Mw 7.5) 18:05:12", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
)

# Dataframe 3: Onda expansiva mayor (4 puntos)
puntos3_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
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
  geom_sf(data = puntos_sf3, color = "#FFFF00", size = 60.5, alpha = 0.1) + 
  geom_sf(data = puntos_sf2, color = "red", size = 30.5, alpha = 0.2) + 
  geom_sf(data = puntos_sf, color = "#5f0404", size = 0.5, alpha = .8) + 
  
  # Capa 4: Etiquetas repelentes configuradas para despejar el territorio nacional
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre),
    min.segment.length = 0,       # Fuerza a que siempre aparezca la línea de conexión
    segment.color = "red",        
    segment.size = 0.4,           
    
    box.padding = 0.6,            
    point.padding = NA,           # CORREGIDO: Elimina el espacio vacío para que la línea toque el punto
    label.padding = 0.18,         
    force = 2.0,                  
    nudge_y = c(0.9, 0.5, 0.7, 1.1), 
    direction = "both",           
    
    color = "black",              
    fill = "white",               
    fontface = "bold",            
    size = 2.2,                   
    max.overlaps = Inf            
  ) +
  
  # OBLIGATORIO: Forzar el sistema de coordenadas espacial y evitar recortes
  coord_sf(clip = "off") +
  
  # Título dinámico adaptado
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
    
    plot.margin = margin(0, 0, 0, 0),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    
    # Caja del título principal adaptada con ggtext
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

# Imprimir el mapa resultante
print(A)



B <- ggdraw(A) +
  draw_text(
    x = 0.25, y = 0.2,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("La Guaira ha sido la zona más afectada y declarada oficialmente como la Zona cero del desastre.", 17)
  )

B


C <- ggdraw(B) +
  draw_text(
    x = 0.6, y = 0.8,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Las cifras oficiales no son claras", 17)
  )

C


D <- ggdraw(C) +
  draw_text(
    x = 0.3, y = 0.02,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Datos: ONU y registros de datos de organizaciones locales. @programandoenR", 90)
  )

D

E <- ggdraw(D) +
  draw_text(
    x = 0.3, y = 0.02,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Datos: ONU y registros de datos de organizaciones locales. @programandoenR", 90)
  )

E




