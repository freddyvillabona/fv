{
  library("ggplot2")
  library("dplyr")
  library("hrbrthemes")
  library("viridis")
  library("readxl")
  library("patchwork") 
  library("viridisLite")
  library(ggplot2)
  library(cowplot)
  library(magick)
  source("lib.R")
}

url_logo <- image_read("a.png")

# 1. Creación del dataset con los datos exactos (8 puntos de registro por métrica)
datos <- data.frame(
  Mes = factor(rep(c("December", "January", "February", "March", "April", "May(1-15)", "May(16-31)", "June"), 3),
               levels = c("December", "January", "February", "March", "April", "May(1-15)", "May(16-31)", "June")),
  Metrica = rep(c("Followers", "Following", "Posts"), each = 8),
  Cantidad = c(
    # Seguidores (8 elementos)
    1031, 1030, 1029, 1005, 1001, 948, 882, 882,
    # Seguidos (8 elementos)
    963, 995, 993, 989, 989, 927, 718, 719,
    # Publicaciones (8 elementos)
    409, 379, 239, 239, 191, 40, 0, 41
  ),
  # Ajuste vertical adaptado para las 3 métricas (8 elementos por bloque, total 24)
  vjust_personalizado = rep(c(-1.5, 1.8, -1.5), each = 8)
)

# 2. Definición de la paleta de colores
colores_personalizados <- c(
  "Followers" = "#2E5B82",    # Azul sutil
  "Following" = "#D0873D",    # Naranja/Dorado
  "Posts" = "#B0413E"         # Rojo/Marrón opaco
)

# 3. Construcción del gráfico con ggplot2
plot <- ggplot(datos, aes(x = Mes, y = Cantidad, group = Metrica, color = Metrica)) +
  
  # Líneas y puntos con los grosores correspondientes
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # Etiquetas de texto con posición dinámica y tipografía en negrita
  geom_text(aes(label = Cantidad, vjust = vjust_personalizado),
            fontface = "bold",
            size = 3.5,
            show.legend = FALSE) +
  
  # Escala de colores manual
  scale_color_manual(values = colores_personalizados, name = "Metrics Analysed") +
  
  # Ajuste de los límites y cortes del eje Y
  scale_y_continuous(limits = c(-50, 1200), breaks = seq(0, 1200, by = 200)) +
  
  # Títulos y etiquetas de los ejes
  labs(
    title = "Time-dependent evolution of the profile (purge pattern)",
    subtitle = "Analysis of correlated metrics",
    caption = "API: Meta-Instagram  X:SuperGrok",
    x = "Sequential (Chronological) Logging Points",
    y = "Absolute Quantity"
  ) +
  
  # Personalización estética del tema
  theme_minimal() +
  theme(
    # NUEVO: Resalta el caption en negrita, aumenta tamaño a 10 y lo pinta de gris oscuro
    plot.caption = element_text(face = "bold", size = 11, color = "#333333", margin = margin(t = 15)),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(face = "italic", size = 10, hjust = 0.5, color = "#555555", margin = margin(b = 20)),
    axis.title.x = element_text(face = "bold", size = 10, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 10, margin = margin(r = 15)),
    axis.text = element_text(color = "#777777", size = 9),
    
    # Cuadrícula horizontal y vertical segmentada (Cambiado a gris claro para que sea visible)
    panel.grid.major = element_line(color = "#FFFFFF", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    
    # Configuración de la leyenda en la parte inferior (TAMAÑOS AMPLIADOS)
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 14, face = "bold"), # Incrementado de 10 a 12
    legend.text = element_text(size = 14),                 # Incrementado de 9 a 11
    legend.spacing.x = unit(0.3, "cm"),                    # Espacio horizontal entre elementos
    
    # Margen general del lienzo
    plot.margin = margin(20, 20, 20, 20)
  ) 

# PASO 3: Fusionar el gráfico con el logotipo
grafico_final <- cowplot::ggdraw() +
  cowplot::draw_plot(plot) +
  cowplot::draw_image(url_logo, x = 0.85, y = -0.04, width = 0.12, height = 0.12)

grafico_final 
