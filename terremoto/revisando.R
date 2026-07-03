library("rvest")
library("sf")
library("ggplot2")
library("tmap")
library("unikn")
library("dplyr")
library("ggrepel")
library("tidyverse")
library("showtext")
library("ggtext")
library("glue")

# Limpieza de entorno limpia
rm(list = ls(pattern = "puntos|coordenadas"))

# Configuración estética
bg_col <- "#CCCCCC"
text_col <- "#151C28"
font_add_google("Oswald", "Oswald")
font_add_google("Nunito", "Nunito")
showtext_auto()
showtext_opts(dpi = 300)

title_font <- "Oswald"
body_font <- "Nunito"

title_text <- glue("<span style='font-family:{title_font};font-size:15pt;'>**Doblete sísmico en Venezuela [06-24-2026]**</span>")

# Carga del mapa (Asegúrate de tener el archivo en tu directorio)
map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1) %>% 
  st_as_sf()

# Dataframe único de puntos (simplifica el código al tener las mismas coordenadas)
puntos_terremoto <- data.frame(
  nombre = c("Sismo precursor (Mw 7.2) 18:04", "Sismo principal (Mw 7.5) 18:05", "Caracas", "La Guaira - Zona cero"),
  lat = c(10.43, 10.401, 10.4880, 10.60),    
  long = c(-68.62, -68.321, -66.9030, -66.93)  
)

# Conversión espacial al CRS nativo de los datos del mapa
puntos_sf <- st_as_sf(puntos_terremoto, coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = st_crs(dataNew))

# Extracción de coordenadas proyectadas para ggrepel
coordenadas <- as.data.frame(st_coordinates(puntos_sf))
puntos_sf$X <- coordenadas$X
puntos_sf$Y <- coordenadas$Y

# Construcción del Gráfico
A <- ggplot() +
  # Capa 1: Fondo y división municipal estilizada
  geom_sf(data = dataNew, aes(fill = N), show.legend = FALSE, size = 0.05) +
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.05) +
  
  # Capa 2: Simulación de ondas expansivas (Círculos concéntricos usando el set proyectado)
  geom_sf(data = puntos_sf, color = "#FFFF00", size = 25, alpha = 0.1) + 
  geom_sf(data = puntos_sf, color = "red", size = 12, alpha = 0.2) + 
  geom_sf(data = puntos_sf, color = "#5f0404", size = 2, alpha = 0.8) + 
  
  # Capa 3: Etiquetas repelentes dinámicas
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre),
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.4,           
    box.padding = 0.8,            
    point.padding = NA,           
    label.padding = 0.2,         
    force = 3.0,                  
    direction = "both",           
    color = "black",              
    fill = "white",               
    fontface = "bold",            
    size = 2.5,                   
    max.overlaps = Inf            
  ) +
  
  # Capa 4: Nota de texto del NYTimes integrada correctamente vía annotate
  annotate(
    "text",
    x = max(puntos_sf$X) + 1.5, # Ajuste dinámico de posición según tus coordenadas
    y = min(puntos_sf$Y) - 0.5,
    label = str_wrap("El epicentro de la destrucción causada por los dos sismos del 24 de junio se encuentra en el estado Vargas, donde está ubicado el Aeropuerto Internacional Simón Bolívar de Maiquetía. Dos de sus tres pistas ya fueron despejadas para vuelos de ayuda y rescate, pero expertos estiman que las operaciones comerciales probablemente no se reanuden durante varios meses. NYTimes", 25),
    size = 0.2,
    hjust = 0.2,
    colour = text_col,
    family = body_font
  ) +
  
  coord_sf(clip = "off") +
  labs(x = NULL, y = NULL, title = title_text) +
  theme_minimal(base_size = 10, base_family = body_font) +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      margin = margin(b = 15, t = 5), 
      family = title_font,
      maxwidth = 0.95 
    )
  )

# Desplegar mapa en consola/plots
print(A)
