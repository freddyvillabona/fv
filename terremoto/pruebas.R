
source("melissa.R")

B <- ggdraw(A) +
  draw_text(
    x = 0.2, y = 0.2,
    size = 8,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("Las réplicas pueden ocurrir días, semanas o incluso años después del primer sismo. Estos sucesos pueden ser de una magnitud igual o mayor que el sismo inicial, y pueden seguir afectando a lugares ya dañados. NYTimes", 25) +
      theme(
        plot.margin = unit(c(0, 0, 0, 0), "cm")
      )
  )

ggdraw(B) +
  draw_text(
    x = 0.7, y = 0.7,
    size = 5,
    hjust = 0,
    colour = text_col,
    family = body_font,
    text = str_wrap("El epicentro de la destrucción causada por los dos sismos del 24 de junio se encuentra en el estado Vargas, donde está ubicado el Aeropuerto Internacional Simón Bolívar de Maiquetía. Dos de sus tres pistas ya fueron despejadas para vuelos de ayuda y rescate, pero expertos estiman que las operaciones comerciales probablemente no se reanuden durante varios meses. NYTimes", 25)
  )
