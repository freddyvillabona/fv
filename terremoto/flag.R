library(ggplot2)
library(ggimage)

# 1. Tus datos con los enlaces directos a las banderas
datos <- data.frame(
  pais = c("Colombia", "España", "Argentina", "México"),
  valor = c(25, 40, 30, 50),
  # Enlaces directos a las imágenes de las banderas
  bandera = c(
    "https://flagcdn.com/w40/ve.png",
    "https://flagcdn.com/w40/ve.png"",
    "https://flagcdn.com/w40/ve.png",
    "https://flagcdn.com/w40/ve.png"))
  )
)

# 2. Hacer el gráfico de barras con las banderas al final de cada barra
ggplot(datos, aes(x = valor, y = reorder(pais, valor))) +
  geom_col(fill = "darkblue", width = 0.6) +
  # ESTA LÍNEA DIBUJA LAS BANDERAS AUTOMÁTICAMENTE
  geom_image(aes(image = bandera), size = 0.05, hjust = -0.2) +
  theme_minimal()
