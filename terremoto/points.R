library(sf)
library(ggplot2)
library(ggrepel)

# 1. Crear el objeto y asegurar la proyección
puntos_terremoto <- data.frame(
  nombre = c("Sismo Precursor (Mw 7.2) 18:04", "Sismo Principal (Mw 7.5) 18:05"),
  lat = c(10.43, 10.401),    # Coordenadas de latitud correctas
  long = c(-68.62, -68.321)  # Coordenadas de longitud correctas
)


puntos_sf <- st_as_sf(puntos_terremoto, coords = c("long", "lat"), crs = 4326)
puntos_sf <- st_transform(puntos_sf, crs = st_crs(dataNew))

# 2. EXTRAER COORDENADAS EXPLÍCITAS (Esto evita el error del objeto no encontrado)
coordenadas <- as.data.frame(st_coordinates(puntos_sf))
puntos_sf$X <- coordenadas$X
puntos_sf$Y <- coordenadas$Y

# 3. Graficar con el mapeo directo de X e Y
A <- ggplot() +
  # Capa del mapa base
  geom_sf(data = dataNew, aes(fill=N), show.legend = TRUE, size = 0.05) +
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.03) +
  
  # Capa de los puntos
  geom_sf(data = puntos_sf, color = "red", size = 4, alpha = 0.9) + 
  
  # Capa de Línea + Texto corregida
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre), # Usamos las coordenadas numéricas extraídas
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.6,           
    box.padding = 0.6,            # Un poco más de espacio para la línea
    color = "black",              
    fill = "white",               
    fontface = "bold",
    size = 1
  ) +
  
  # Estética y textos
  ggtitle("TERREMOTOS EN VENEZUELA") +
  xlab("") +
  ylab("") +
  theme_void() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16)) 
#  labs(fill= "Відсоток")

A
