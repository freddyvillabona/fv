{
library("rvest")
library('sf')
library("ggplot2")
library("tmap")
library("unikn")
library("dplyr")
library(ggrepel)
}

map <- read_sf('Municipios_Venezuela.shp')

dataNew <- map %>% 
  mutate(N = 1)

dataNew <- dataNew %>% 
  st_as_sf()

data <- filter(map, ESTADO == "Vargas" )

data_sf <- data %>% 
  st_as_sf()

map <- ggplot(data = map) +
  ggtitle("Україна - Відсоток російськомовних") +
  xlab("") +
  ylab("") +
  geom_sf(aes(fill=ID), show.legend = T, size = 0.05) +
  scale_fill_viridis_c(alpha = .6) +
  #geom_sf_text(aes(label =Ucraniano),size=3,family="sans")+
  # geom_label(aes(fill = Ucraniano), colour = "white", fontface = "bold") +
  #theme_void() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 20)) +
  labs(fill= "Відсоток")

map


A <- ggplot(data = dataNew) +
  ggtitle("TERREMOTOS EN VENEZUELA") +
  xlab("") +
  ylab("") +
  geom_sf(aes(fill=N), show.legend = T, size = 0.05) +
  geom_sf(fill = "black", alpha = 0.6, color = "white", size = 0.05) +
  geom_sf(data = puntos, color = "red", size = 1, alpha = 0.8) + 
  #geom_col(fill = "#3182bd", alpha = 0.6) +
 # scale_fill_viridis_c(alpha = .6) +
  #geom_sf_text(aes(label =Ucraniano),size=3,family="sans")+
  # geom_label(aes(fill = Ucraniano), colour = "white", fontface = "bold") +
  #theme_void() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 20)) +
  labs(fill= "Відсоток") +
  geom_label_repel(
    data = puntos,
    aes(label = nombre, geometry = geometry),
    stat = "sf_coordinates",      # Extrae automáticamente X e Y de la geometría
    min.segment.length = 0,       # Fuerza a que siempre se dibuje la línea conectora
    segment.color = "red",        # Color de la línea conectora
    segment.size = 1.6,           # Grosor de la línea
    box.padding = 0.6,            # Espacio alrededor del texto para alejarlo del punto
    color = "black",              # Color de las letras
    fill = "white",               # Fondo del cuadro de texto para que sea legible
    fontface = "bold",
    size = 1.5
  ) +
  theme_void()

A

plot(st_geometry(puntos), col = "red", pch = 19, add = TRUE)

# add new variable

dataNew <- map %>% 
  mutate(N = 1)
