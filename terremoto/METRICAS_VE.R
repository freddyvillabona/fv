# 1. Cargar la librería necesaria
library(ggplot2)

# 2. Estructurar los datos CORREGIDOS
datos <- data.frame(
  Metrica = c("Desaparecidos",
              "Hospitalizados",
              "Heridos", 
              "Muertes",
              "Edificios afectados", # <- Se agregó la coma faltante aquí
              "Réplicas"),
  
  Valor = c(72200, 22000, 5400, 1927, 1307, 512), 
  
  # Se alinearon las etiquetas para que coincidan con el vector 'Valor'
  Etiqueta = c("72.200", "22.000", "5.400", "1.927", "878", "512"),
  
  ColorHex = c("#4A0000", "#6A0000", "#8A0000", "#B00000", "#D50000", "#FF0000")
)

# Forzar el orden vertical exacto en el que escribiste las métricas (de arriba a abajo)
datos$Metrica <- factor(datos$Metrica, levels = rev(datos$Metrica))

# 3. Construcción del gráfico optimizado
ggplot(datos, aes(x = Valor, y = Metrica, fill = Metrica)) +
  # Dibujar las barras horizontales
  geom_col(width = 0.5, show.legend = FALSE) +
  
  # Inyectar la paleta de colores personalizada de forma exacta
  scale_fill_manual(values = setNames(datos$ColorHex, datos$Metrica)) +
  
  # Añadir las etiquetas numéricas a la derecha de cada barra con formato legible
  geom_text(aes(label = Etiqueta), 
            hjust = -0.2, 
            color = "black", 
            size = 4, 
            fontface = "bold") +
  
  # Dar espacio extra proporcional a la derecha para que la cifra "72200" no se corte
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  # Títulos adaptados al reporte sísmico de Venezuela
  labs(
    title = "Reporte de daños e impacto humano",
    subtitle = "Balance estadístico de los eventos sísmicos en Venezuela",
    x = NULL,
    y = NULL
  ) +
  
  # Estilo visual limpio y corporativo
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#111625"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 11, face = "bold", color = "#111625", hjust = 1),
    axis.text.x = element_blank(),    # Ocultar la escala numérica inferior
    panel.grid = element_blank(),     # Eliminar cuadrículas grises de fondo
    plot.margin = margin(t = 20, r = 40, b = 20, l = 20, unit = "pt") # Margen de respiro
  )
