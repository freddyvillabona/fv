{
  source("h.R")
}


A <- ggplot() +
  # Capa Base 1: Fondo de los municipios coloreados
  geom_sf(data = dataNew, fill = "black", alpha = 0.25, color = "white", size = 0.05) 
 # geom_sf(data = dataNew, aes(fill = N), show.legend = FALSE, size = 0.05) +
  
  # Capa Base 2: Máscara oscura y estilizada con bordes blancos para los municipios
  geom_sf(data = dataNew, fill = "black", alpha = 0.6, color = "white", size = 0.05)+
  geom_sf(data = puntos_sf3, color = "#FFFF00", size = 60.5, alpha = 0.1) + 
  geom_sf(data = puntos_sf2, color = "red", size = 30.5, alpha = 0.2) + 
  geom_sf(data = puntos_sf, color = "#5f0404", size = 0.5, alpha = .8)  +
  # Evitar recortes de las cajas de texto en los bordes del mapa
  coord_sf(clip = "off") +
  
  # Estructura de títulos
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
    
    plot.margin = margin(15, 15, 15, 15), 
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
  ) +
  geom_label_repel(
    data = puntos_sf,
    aes(x = X, y = Y, label = nombre),
    min.segment.length = 0,       
    segment.color = "red",        
    segment.size = 0.4,           
    
    box.padding = 0.5,            
    point.padding = 0.2,          
    label.padding = 0.2,         
    force = 50.0,                 # Forzar el reposicionamiento final
    
    # Mapeo de los vectores adaptados a la proyección métrica
    nudge_x = puntos_sf$empuje_x,
    nudge_y = puntos_sf$empuje_y,
    direction = "both",           
    
    color = "black",              
    fill = "white",               
    fontface = "bold",            
    size = 2.2,                   
    max.overlaps = Inf            
  )

  

A
