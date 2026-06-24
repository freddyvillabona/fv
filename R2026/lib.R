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
  library(tidyverse)
  library(showtext)
  library(ggtext)
  library(nrBrand)
  library(glue)
  library(ggview)
  library(ggh4x)
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