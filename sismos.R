
library(ggplot2)
library(scales)

# 2. Crear el dataset con los datos exactos de la imagen
datos_mundial <- data.frame(
  Pais = factor(
    c("Valdivia, Chile", "Portugal", "England", "Argentina", "France", "Netherlands", "Croatia", "Morocco"),
    levels = c("Valdivia, Chile", "Portugal", "England", "Argentina", "France", "Netherlands", "Croatia", "Morocco") # Mantiene el orden
  ),
  Porcentaje = c(9.5, 14.10, 14.05, 13.20, 13.10, 7.10, 4.00, 1.35)
)

# 3. Construir el gráfico
ggplot(datos_mundial, aes(x = Pais, y = Porcentaje, fill = Porcentaje)) +
  # Crear las barras numéricas
  geom_col(width = 0.85, show.legend = FALSE) +
  
  # Añadir el texto de los porcentajes arriba de cada barra
  geom_text(
    aes(label = sprintf("%.2f %%", Porcentaje)), 
    vjust = -0.5, 
    size = 3.5, 
    color = "gray30"
  ) +
  
  # Paleta de colores degradada (Azul oscuro -> Verde brillante)
  scale_fill_gradient(low = "#99cc33", high = "#2e5c8a") +
  
  # Configurar el eje Y (límites y quiebres cada 10)
  scale_y_continuous(
    limits = c(0, 10), 
    breaks = seq(0, 10, by = 10),
    expand = c(0, 0)
  ) +
  
  # Títulos y etiquetas de los ejes
  labs(
    title = "Terremotos de mayor magnitud",
    subtitle = "Datos:X:@nytimes",
    x = "País",
    y = "Magnitud (Mw)"
  ) +
  
  # Aplicar un estilo limpio y minimalista
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 20)),
    axis.title.x = element_text(hjust = 1, size = 10, face = "italic"),
    axis.title.y = element_text(hjust = 1, size = 10, face = "italic", margin = margin(r = 10)),
    panel.grid.major.x = element_blank(), # Elimina líneas verticales de fondo
    panel.grid.minor = element_blank(),   # Elimina líneas secundarias de fondo
    panel.grid.major.y = element_line(color = "gray92"),
    axis.line.x = element_line(color = "black")
  )
