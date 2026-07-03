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

# Definimos el título limpio con ggtext
title_text <- glue("<span style='font-family:{title_font};font-size:15pt;'>**Doblete sísmico en Venezuela [06-24-2026]<br>**</span>")

# Caja de información izquierda en formato HTML para el pie de página
nota_izquierda <- glue("
  <span style='font-size:7.5pt; color:#151C28; line-height:1.2;'>
  <b>ESTADO DEL SISTEMA:</b> Monitoreo de réplicas activo a nivel nacional. Área de cobertura: Región Capital y Central.
  </span>
")

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
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.4,           
    box.padding = 0.6,            
    point.padding = NA,           
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
  
  coord_sf(clip = "off") +
  
  # Inyectamos la información inferior izquierda directamente en la sección caption nativa
  labs(
    x = NULL, y = NULL,
    title = title_text,
    caption = nota_izquierda
  ) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(15, 5, 10, 5), 
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
    # Modificamos el caption para que actúe como una caja de texto líquida y autoadaptable
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 15),
      family = body_font,
      fill = "white",                     # Fondo blanco para simular la caja exterior
      box.colour = highlight_col,          # Borde de color violeta de tu paleta
      box.size = 0.6,                     # Grosor fino para evitar saturación gráfica
      padding = margin(6, 8, 6, 8),       # Margen interno de la caja de texto
      r = unit(3, "pt"),                  # Bordes ligeramente redondeados nativos
      maxwidth = 0.65                     # Restringe el ancho al 65% para que se quede estrictamente a la izquierda
    )
  )

# --- CREACIÓN DEL PANEL DE TEXTO LATERAL DERECHO CON DISEÑO COMPACTO ---

texto_informativo <- glue("
  <span style='font-family:{title_font}; font-size:8pt; color:{highlight_col};'>**ANÁLISIS DEL EVENTO**</span><br><br>
  <span style='font-size:8pt; color:{text_col}; line-height:1.35;'>
  El fenómeno registrado corresponde a un **doblete sísmico**, caracterizado por la ocurrencia secuencial de dos eventos de magnitud severa en un lapso inferior a un minuto.<br><br>
  Los epicentros se localizaron instrumentalmente en la región centro-occidental (Estado Yaracuy), asociados al sistema de fallas.<br><br>
  Las ciudades de **Caracas** y **La Guaira** se incluyen exclusivamente como referencias de control espacial urbano.
  </span>
")

texto_caja_inferior <- glue("
  <span style='font-size:7pt; color:white; line-height:1.2;'>
  **NOTA TÉCNICA:**<br>
  Datos obtenidos e integrados bajo las coordenadas oficiales del USGS internacionales.
  </span>
")

panel_texto <- ggplot() +
  theme_void() +
  labs(title = texto_informativo) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(t = 50, r = 15, b = 15, l = 10), 
    plot.title = element_textbox_simple(
      family = body_font,
      maxwidth = 0.95,
      lineheight = 1.3
    )
  ) +
  # Se simplifica el annotate quitando unidades pesadas npc para blindar el visor
  annotate(
    geom = "richtext",
    x = 0, y = 0.05,            
    label = texto_caja_inferior,
    family = body_font,
    fill = "#2C3E50",           
    color = "white",            
    label.padding = unit(0.4, "lines"),
    label.r = unit(0.1, "lines"),
    hjust = 0, vjust = 0
  )

# --- ENSAMBLADO MULTICOLUMNA DE LA COMPOSICIÓN ---
# Se unifica el mapa y el bloque explicativo lateral de forma directa
grafico_final <- plot_grid(A, panel_texto, rel_widths = c(3.2, 1), nrow = 1)

# Imprimir el gráfico final sin dependencias de Viewport
print(grafico_final)
