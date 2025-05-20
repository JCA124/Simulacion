library(shiny)
library(kableExtra)
library(data.table)
library(ggplot2)
library(shinycssloaders)

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
  
  # Actualizar límites con botones de infinito
  observeEvent(input$inf_neg, {
    updateTextInput(session, "lim_inf", value = "-Inf")
  })
  
  observeEvent(input$inf_pos, {
    updateTextInput(session, "lim_sup", value = "Inf")
  })
  
  # Generar números aleatorios para Números Aleatorios
  datos_aleatorios <- eventReactive(input$mostrar, {
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
  # Procesar límites
  limites <- eventReactive(input$calcular, {
    lim_inf <- tryCatch(as.numeric(input$lim_inf), error = function(e) -Inf, warning = function(w) -Inf)
    lim_sup <- tryCatch(as.numeric(input$lim_sup), error = function(e) Inf, warning = function(w) Inf)
    if (tolower(input$lim_inf) == "-inf") lim_inf <- -Inf
    if (tolower(input$lim_sup) == "inf") lim_sup <- Inf
    list(lim_inf = lim_inf, lim_sup = lim_sup)
  })
  
  # Función para evaluar
  f <- function(x) {
    tryCatch(eval(parse(text = input$funcion)), error = function(e) NA)
  }
  
  # Generar puntos para el gráfico del área
  coords_area <- eventReactive(input$calcular, {
    req(input$calcular)
    lim_inf <- limites()$lim_inf
    lim_sup <- limites()$lim_sup
    
    if (is.infinite(lim_inf) && is.infinite(lim_sup)) {
      x_vals <- seq(-5, 5, length.out = 100) # Para [-∞, ∞]
    } else if (is.infinite(lim_inf)) {
      x_vals <- seq(lim_sup - 10, lim_sup, length.out = 100) # Para [-∞, b]
    } else if (is.infinite(lim_sup)) {
      x_vals <- seq(lim_inf, lim_inf + 10, length.out = 100) # Para [a, ∞]
    } else {
      x_vals <- seq(min(lim_inf, lim_sup), max(lim_inf, lim_sup), length.out = 100)
    }
    
    y_vals <- sapply(x_vals, f)
    if (all(is.na(y_vals))) {
      return(NULL)
    }
    data.frame(x = x_vals, y = y_vals)
  })
  
  # Calcular integral por Monte Carlo
  monte_carlo <- eventReactive(input$calcular, {
    req(input$calcular)
    lim_inf <- limites()$lim_inf
    lim_sup <- limites()$lim_sup
    metodo_aleatorio <- if (input$metodo_int == "Multiplicativo") {
      function(...) numeros_multiplicativo(...)
    } else {
      function(...) numeros_mixto(..., c = input$incremento)
    }
    secuencia <- seq(100, 10000, by = 250)
    
    if (is.finite(lim_inf) && is.finite(lim_sup)) {
      # Caso [a, b]
      if (lim_inf <= lim_sup) {
        aprox <- sapply(secuencia, function(k) {
          u <- metodo_aleatorio(a = 7^5, m = 2^31 - 1, semilla = as.numeric(Sys.time()), cantidad = k)
          x <- lim_inf + (lim_sup - lim_inf) * u
          valores <- sapply(x, f)
          if (all(is.na(valores)) || any(is.infinite(valores))) return(NA)
          mean(valores, na.rm = TRUE) * (lim_sup - lim_inf)
        })
      } else {
        # Caso [a, b] con a > b
        aprox <- sapply(secuencia, function(k) {
          u <- metodo_aleatorio(a = 7^5, m = 2^31 - 1, semilla = as.numeric(Sys.time()), cantidad = k)
          x <- lim_sup + (lim_inf - lim_sup) * u
          valores <- sapply(x, f)
          if (all(is.na(valores)) || any(is.infinite(valores))) return(NA)
          -mean(valores, na.rm = TRUE) * (lim_inf - lim_sup)
        })
      }
      teorico <- tryCatch(integrate(f, lower = min(lim_inf, lim_sup), upper = max(lim_inf, lim_sup))$value, error = function(e) NA)
      if (lim_inf > lim_sup) teorico <- -teorico
    } else if (is.finite(lim_inf) && is.infinite(lim_sup)) {
      # Caso [a, ∞]
      aprox <- sapply(secuencia, function(k) {
        u <- metodo_aleatorio(a = 7^5, m = 2^31 - 1, semilla = as.numeric(Sys.time()), cantidad = k)
        x <- lim_inf - log(1 - u + 1e-10)
        valores <- sapply(x, f) / dexp(x - lim_inf)
        if (all(is.na(valores)) || any(is.infinite(valores))) return(NA)
        mean(valores, na.rm = TRUE)
      })
      teorico <- tryCatch(integrate(f, lower = lim_inf, upper = Inf)$value, error = function(e) NA)
    } else if (is.infinite(lim_inf) && is.finite(lim_sup)) {
      # Caso [-∞, b]
      aprox <- sapply(secuencia, function(k) {
        u <- metodo_aleatorio(a = 7^5, m = 2^31 - 1, semilla = as.numeric(Sys.time()), cantidad = k)
        x <- lim_sup + log(u + 1e-10)
        valores <- sapply(x, f) / dexp(lim_sup - x)
        if (all(is.na(valores)) || any(is.infinite(valores))) return(NA)
        mean(valores, na.rm = TRUE)
      })
      teorico <- tryCatch(integrate(f, lower = -Inf, upper = lim_sup)$value, error = function(e) NA)
    } else {
      # Caso [-∞, ∞]
      aprox <- sapply(secuencia, function(k) {
        u <- metodo_aleatorio(a = 7^5, m = 2^31 - 1, semilla = as.numeric(Sys.time()), cantidad = k)
        x <- qnorm(u, mean = 0, sd = 1)
        valores <- sapply(x, f) / dnorm(x)
        if (all(is.na(valores)) || any(is.infinite(valores))) return(NA)
        mean(valores, na.rm = TRUE)
      })
      teorico <- tryCatch(integrate(f, lower = -Inf, upper = Inf)$value, error = function(e) NA)
    }
    
    list(aprox = aprox, teorico = teorico, secuencia = secuencia)
  })
  
  # Gráfico del área bajo la curva
  output$graf_area <- renderPlot({
    req(input$calcular)
    datos <- coords_area()
    if (is.null(datos)) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Función inválida o no evaluable", size = 5) +
        theme_minimal()
    } else {
      lim_inf <- limites()$lim_inf
      lim_sup <- limites()$lim_sup
      
      p <- ggplot(datos, aes(x = x, y = y)) +
        geom_line(color = "blue", linewidth = 1) +
        labs(title = paste("Área bajo f(x) =", input$funcion),
             x = "x", y = "f(x)") +
        theme_minimal()
      
      if (is.finite(lim_inf) && is.finite(lim_sup)) {
        x_fill <- datos$x[datos$x >= min(lim_inf, lim_sup) & datos$x <= max(lim_inf, lim_sup)]
        y_fill <- datos$y[datos$x >= min(lim_inf, lim_sup) & datos$x <= max(lim_inf, lim_sup)]
        if (length(x_fill) > 0 && length(y_fill) > 0) {
          p <- p + geom_area(data = data.frame(x = x_fill, y = y_fill), aes(x = x, y = y), fill = "lightblue", alpha = 0.3) +
            geom_vline(xintercept = lim_inf, linetype = "dashed", color = "red") +
            geom_vline(xintercept = lim_sup, linetype = "dashed", color = "red")
        }
      }
      
      p
    }
  })
  
  # Gráfico de aproximación
  output$graf_aprox <- renderPlot({
    req(input$calcular)
    mc <- monte_carlo()
    datos <- data.table(
      Aproximacion = mc$aprox,
      Teorico = mc$teorico,
      Aleatorios = mc$secuencia
    ) %>% 
      melt(id.vars = "Aleatorios", variable.name = "Etiqueta", value.name = "Valor")
    
    datos <- datos[!is.na(Valor) & is.finite(Valor)]
    
    if (nrow(datos) == 0) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No se pueden calcular aproximaciones válidas", size = 5) +
        theme_minimal()
    } else {
      ggplot(datos, aes(x = Aleatorios, y = Valor, color = Etiqueta)) +
        geom_line() +
        labs(title = "Aproximación de Monte Carlo",
             x = "Número de puntos",
             y = "Valor de la integral",
             caption = paste("Valor teórico:", ifelse(is.na(mc$teorico), "No disponible", round(mc$teorico, 4)))) +
        theme_minimal()
    }
  })
}