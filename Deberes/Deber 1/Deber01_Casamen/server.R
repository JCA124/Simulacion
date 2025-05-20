library(shiny)
library(kableExtra)
library(data.table)
library(ggplot2)

# Método Congruencial Multiplicativo
numeros_multiplicativo <- function(a, m, semilla, cantidad) {
  resultado <- numeric(cantidad + 1)
  resultado[1] <- semilla
  for (k in 2:length(resultado)) {
    resultado[k] <- (a * resultado[k - 1]) %% m
  }
  return(resultado[-1] / m)
}

# Método Congruencial Mixto
numeros_mixto <- function(a, c, m, semilla, cantidad) {
  resultado <- numeric(cantidad + 1)
  resultado[1] <- semilla
  for (k in 2:length(resultado)) {
    resultado[k] <- (a * resultado[k - 1] + c) %% m
  }
  return(round(resultado[-1] / m, 6))
}

# Crear tabla con filas y columnas
crear_tabla <- function(numeros, filas, columnas) {
  total_celdas <- filas * columnas
  tabla <- rep(NA_real_, total_celdas)
  tabla[1:min(length(numeros), total_celdas)] <- numeros[1:min(length(numeros), total_celdas)]
  tabla <- as.data.frame(matrix(tabla, nrow = filas, ncol = columnas, byrow = TRUE))
  colnames(tabla) <- paste0("Col", 1:columnas)
  return(tabla)
}

# Lógica del servidor
function(input, output, session) {
  
  # Generar números aleatorios
  datos_aleatorios <- reactive({
    list(
      multiplicativo = numeros_multiplicativo(a = input$constante, m = input$divisor, semilla = input$semilla, cantidad = input$cantidad),
      mixto = numeros_mixto(a = input$constante, c = input$incremento, m = input$divisor, semilla = input$semilla, cantidad = input$cantidad)
    )
  })
  
  # Tabla Multiplicativo
  output$tabla_multiplicativo <- function() {
    req(input$mostrar)
    if (input$metodo %in% c("Multiplicativo", "Ambos")) {
      tabla <- crear_tabla(datos_aleatorios()$multiplicativo, input$filas, input$columnas)
      kbl(tabla, booktabs = TRUE, escape = FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#1D3889", color = "#ffffff") %>% 
        scroll_box(width = "100%", height = "200px")
    }
  }
  
  # Tabla Mixto
  output$tabla_mixto <- function() {
    req(input$mostrar)
    if (input$metodo %in% c("Mixto", "Ambos")) {
      tabla <- crear_tabla(datos_aleatorios()$mixto, input$filas, input$columnas)
      kbl(tabla, booktabs = TRUE, escape = FALSE) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = c("bordered"), font_size = 12) %>%
        row_spec(0, background = "#1D3889", color = "#ffffff") %>% 
        scroll_box(width = "100%", height = "200px")
    }
  }
  
  # Histograma Multiplicativo
  output$hist_multiplicativo <- renderPlot({
    req(input$mostrar)
    if (input$metodo %in% c("Multiplicativo", "Ambos")) {
      ggplot(data.frame(x = datos_aleatorios()$multiplicativo), aes(x = x)) +
        geom_histogram(bins = 20, fill = "lightblue", color = "black") +
        labs(title = "Histograma Multiplicativo", x = "Valores", y = "Frecuencia") +
        theme_minimal()
    }
  })
  
  # Histograma Mixto
  output$hist_mixto <- renderPlot({
    req(input$mostrar)
    if (input$metodo %in% c("Mixto", "Ambos")) {
      ggplot(data.frame(x = datos_aleatorios()$mixto), aes(x = x)) +
        geom_histogram(bins = 20, fill = "lightgreen", color = "black") +
        labs(title = "Histograma Mixto", x = "Valores", y = "Frecuencia") +
        theme_minimal()
    }
  })
  
  # Pestaña Integrales
  coords <- eventReactive(input$calcular, {
    x_vals <- seq(input$lim_inf, input$lim_sup, length.out = 100)
    y_vals <- sapply(x_vals, function(x) { eval(parse(text = input$funcion)) })
    data.frame(x = x_vals, y = y_vals)
  })
  
  output$graf_fun01 <- renderPlot({
    req(input$calcular)
    f <- function(x) eval(parse(text = input$funcion))
    datos <- coords()
    delta_x <- (input$lim_sup - input$lim_inf) / (length(datos$x) - 1)
    area <- (delta_x / 2) * (f(input$lim_inf) + 2 * sum(f(datos$x[2:(length(datos$x) - 1)])) + f(input$lim_sup))
    
    ggplot(datos, aes(x = x, y = y)) + 
      geom_line(color = "blue", linewidth = 1) +
      geom_area(aes(x = x, y = y), fill = "lightblue", alpha = 0.3) +
      geom_vline(xintercept = input$lim_inf, linetype = "dashed", color = "red") +
      geom_vline(xintercept = input$lim_sup, linetype = "dashed", color = "red") +
      labs(title = paste("Área bajo f(x) =", input$funcion),
           subtitle = paste("Intervalo:", input$lim_inf, "a", input$lim_sup),
           x = "x",
           y = "f(x)",
           caption = paste("Área aproximada:", round(area, 4))) + 
      theme_minimal()
  })
  
  output$graf_aprox01 <- renderPlot({
    req(input$calcular)
    secuencia <- seq(100, 10000, by = 250)
    f <- function(x) eval(parse(text = input$funcion))
    
    metodo_aleatorio <- if (input$metodo_int == "Multiplicativo") {
      numeros_multiplicativo
    } else {
      numeros_mixto
    }
    
    if (input$lim_inf == 0 & input$lim_sup == 1) {
      data.table(Aproximacion = sapply(secuencia, function(k) {
        res <- metodo_aleatorio(a = 7^5, m = 2^31 - 1, c = input$incremento, semilla = as.numeric(Sys.time()), cantidad = k)
        mean(sapply(res, function(x) { eval(parse(text = input$funcion)) }))
      }), 
      Teorico = integrate(f, lower = 0, upper = 1)$value) %>% 
        gather(key = "Etiqueta", value = "Valor") %>% 
        mutate(Aleatorios = c(secuencia, secuencia)) %>% 
        ggplot(aes(x = Aleatorios, y = Valor, group = Etiqueta, colour = Etiqueta)) + 
        geom_line()
    } else {
      data.table(Aproximacion = sapply(secuencia, function(k) {
        res <- input$lim_inf + (input$lim_sup - input$lim_inf) * metodo_aleatorio(a = 7^5, m = 2^31 - 1, c = input$incremento, semilla = as.numeric(Sys.time()), cantidad = k)
        mean(sapply(res, function(x) { (input$lim_sup - input$lim_inf) * eval(parse(text = input$funcion)) }))
      }), 
      Teorico = integrate(f, lower = input$lim_inf, upper = input$lim_sup)$value) %>% 
        gather(key = "Etiqueta", value = "Valor") %>% 
        mutate(Aleatorios = c(secuencia, secuencia)) %>% 
        ggplot(aes(x = Aleatorios, y = Valor, group = Etiqueta, colour = Etiqueta)) + 
        geom_line()
    }
  })
}