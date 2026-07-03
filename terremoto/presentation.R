
source("melissa.R")

B <- ggdraw(A) +
  draw_text(
    x = 0.25, y = 0.2,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("La Guaira ha sido la zona más afectada y declarada oficialmente como la Zona cero del desastre.", 17)
  )

B


C <- ggdraw(B) +
  draw_text(
    x = 0.6, y = 0.8,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Las cifras oficiales no son claras", 17)
  )

C



E <- ggdraw(C) +
  draw_text(
    x = 0.3, y = 0.02,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Datos: ONU y otras fuentes locales.", 80)
  )

#E

# PASO 3: Fusionar el gráfico con el logotipo
grafico_final <- cowplot::ggdraw() +
  cowplot::draw_plot(E) +
  cowplot::draw_image(url_logo, x = 0.65, y = -0.04, width = 0.12, height = 0.12)

grafico_final 
