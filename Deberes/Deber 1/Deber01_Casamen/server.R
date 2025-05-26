library(shiny)
library(kableExtra)
library(data.table)
library(tidyverse)
library(ggplot2)
library(DT)

# Función que genera números aleatorios bajo el método congruencial multiplicativo
random_cong <- function(a, m, x0, n) {
  res <- numeric(n+1)
  res[1] <- x0
  for (k in 2:length(res)) {
    res[k] <- (a * res[k-1]) %% m
  }
  return(res[-1] / m)
}

# Función que genera números aleatorios bajo el método congruencial mixto
random_mixt <- function(a, c, m, x0, n) {
  res <- numeric(n+1)
  res[1] <- x0
  for (k in 2:length(res)) {
    res[k] <- (a * res[k-1] + c) %% m
  }
  return(round((res[-1]) / m, 6))
}

conv_matrix <- function(vector, cols = 10) {
  res <- rep(NA_real_, ceiling(length(vector) / cols) * cols)
  res[1:length(vector)] <- vector
  res <- as.data.frame(matrix(res, nrow = ceiling(length(vector) / cols), ncol = cols, byrow = TRUE))
  colnames(res) <- paste0("Cols", 1:cols)
  return(res)
}

# Servidor
function(input, output, session) {
  
  #### Generación números aleatorios #######################################################
  
  coords <- eventReactive(input$calcular, {
    x_vals <- seq(input$lim_inf, input$lim_sup, length.out = 100)
    y_vals <- sapply(x_vals, function(x) { eval(parse(text = input$funcion)) })
    coords <- data.frame(x = x_vals, y = y_vals)
  })
  
  output$tabla <- function() {
    res <- conv_matrix(
      random_cong(
        a = input$constante,
        m = input$divisor,
        x0 = input$semilla,
        n = input$num
      )
    )
    
    kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 6) %>%
      kable_styling(
        full_width = FALSE,
        bootstrap_options = c("bordered", "striped", "hover", "condensed"),
        font_size = 13
      ) %>%
      row_spec(0, background = "#1e81b0", color = "white", bold = TRUE) %>%
      scroll_box(width = "100%", height = "250px")
  }
  
  output$tabla1 <- function() {
    res <- conv_matrix(
      random_mixt(
        a = input$constante,
        c = input$c,
        m = input$divisor,
        x0 = input$semilla,
        n = input$num
      )
    )
    
    kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 6) %>%
      kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover", "condensed", "bordered"),
        font_size = 13
      ) %>%
      row_spec(0, background = "#e28743", color = "white", bold = TRUE) %>% 
      scroll_box(width = "100%", height = "230px")
  }
  
  # Histograma método multiplicativo
  output$hist_multiplicativo <- renderPlot({
    datos <- random_cong(a = input$constante,
                         m = input$divisor,
                         x0 = input$semilla,
                         n = input$num)
    
    hist(datos, 
         breaks = input$barras, 
         col = "#abdbe3", 
         border = "#fff",
         main = "",
         xlab = "Valores generados", 
         ylab = "Densidad",
         freq = FALSE, 
         col.lab = "#34495e",   
         las = 1)
  })
  
  # Histograma método mixto
  output$hist_mixto <- renderPlot({
    datos <- random_mixt(a = input$constante,
                         c = input$c,
                         m = input$divisor,
                         x0 = input$semilla,
                         n = input$num)
    
    hist(datos, 
         breaks = input$barras, 
         col = "#eab676",  
         border = "#ffffff",
         main = "",
         xlab = "Valores generados", 
         ylab = "Densidad",
         freq = FALSE,
         col.lab = "#34495e",
         las = 1)
  })
  
  ######################################### INTEGRALES ############################################################
  
  output$graf_fun01 <- renderPlot({
    a <- input$lim_inf
    b <- input$lim_sup
    limite_infinito <- b > 500
    b_plot <- if (limite_infinito) 100 else b
    
    f_input <- input$funcion
    f <- function(x_val) {
      tryCatch(
        eval(parse(text = f_input), envir = list(x = x_val)),
        error = function(e) NA
      )
    }
    
    x_vals <- seq(a, b_plot, length.out = 100)
    y_vals <- sapply(x_vals, f)
    datos <- data.frame(x = x_vals, y = y_vals)
    
    if (!limite_infinito) {
      delta_x <- (b - a) / (length(x_vals) - 1)
      area <- (delta_x / 2) * (y_vals[1] + 2 * sum(y_vals[2:(length(y_vals) - 1)]) + y_vals[length(y_vals)])
    } else {
      area <- NA
    }
    
    ggplot(datos, aes(x = x, y = y)) +
      geom_line(color = "#2C3E50", linewidth = 1.2) +
      geom_area(fill = "#AEDFF7", alpha = 0.5) +
      geom_vline(xintercept = a, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      geom_vline(xintercept = b_plot, linetype = "dashed", color = "#e74c3c", linewidth = 0.8) +
      labs(
        title = paste("Área bajo la curva de f(x) =", f_input),
        subtitle = if (limite_infinito) "[0, ∞) truncado a 100 para visualización" else paste("Intervalo:", a, "a", b),
        x = "x",
        y = "f(x)",
        caption = if (!is.na(area)) paste("Área aproximada (método del trapecio):", round(area, 4)) else "Área no calculada"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#2C3E50", size = 16),
        plot.subtitle = element_text(size = 13, color = "#34495e"),
        axis.title = element_text(face = "bold", size = 13),
        plot.caption = element_text(size = 11, color = "gray40", hjust = 1),
        panel.grid.minor = element_blank()
      )
  })
  
  output$graf_aprox01 <- renderPlot({
    f_input <- input$funcion
    a <- input$lim_inf
    b <- input$lim_sup
    limite_infinito <- b > 500
    es_divergente <- FALSE
    razon_divergencia <- ""
    
    f <- function(x_val) {
      tryCatch(
        eval(parse(text = f_input), envir = list(x = x_val)),
        error = function(e) NA
      )
    }
    
    # Validación de divergencia
    tryCatch({
      if (!limite_infinito) {
        puntos <- seq(a, b, length.out = 50)
        valores <- sapply(puntos, f)
        
        if (any(is.infinite(valores))) {
          es_divergente <- TRUE
          razon_divergencia <- "La función tiene asíntotas en el intervalo."
        } else if (any(is.na(valores))) {
          es_divergente <- TRUE
          razon_divergencia <- "La función no está definida en todo el intervalo."
        }
      } else {
        val_a <- f(a)
        if (is.infinite(val_a)) {
          es_divergente <- TRUE
          razon_divergencia <- paste("La función tiende a infinito en x =", a)
        }
        grandes_x <- seq(100, 1000, length.out = 10)
        vals_grandes <- sapply(grandes_x, f)
        if (any(is.infinite(vals_grandes))) {
          es_divergente <- TRUE
          razon_divergencia <- "La función tiende a infinito cuando x→∞"
        } else if (mean(abs(vals_grandes), na.rm = TRUE) > 0.1) {
          es_divergente <- TRUE
          razon_divergencia <- "La función no tiende a cero cuando x→∞"
        }
      }
    }, error = function(e) {
      es_divergente <<- TRUE
      razon_divergencia <<- paste("Error al evaluar la función:", e$message)
    })
    
    if (es_divergente) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "INTEGRAL DIVERGENTE", size = 8, color = "red") +
          annotate("text", x = 0.5, y = 0.4, label = razon_divergencia, size = 5, color = "darkred") +
          theme_void() +
          labs(caption = "La integral no converge a un valor finito")
      )
    }
    
    secuencia <- seq(100, 10000, by = 250)
    usar_mixto <- input$metodo == "Congruencial Mixto"
    generar <- function(n) {
      if (usar_mixto) {
        random_mixt(a = 123, c = 7, m = 2^31 - 1, x0 = as.numeric(Sys.time()), n = n)
      } else {
        random_cong(a = 7^5, m = 2^31 - 1, x0 = as.numeric(Sys.time()), n = n)
      }
    }
    
    aprox <- numeric(length(secuencia))
    teorico <- NA
    
    if (!limite_infinito) {
      for (i in seq_along(secuencia)) {
        n <- secuencia[i]
        u <- generar(n)
        x <- a + (b - a) * u
        fx <- sapply(x, f)
        aprox[i] <- mean(fx, na.rm = TRUE) * (b - a)
        if (abs(aprox[i]) > 1e6) {
          es_divergente <- TRUE
          razon_divergencia <- "La aproximación crece sin límite"
          break
        }
      }
      if (!es_divergente) {
        teorico <- tryCatch({
          integrate(f, lower = a, upper = b)$value
        }, error = function(e) NA)
      }
    } else {
      for (i in seq_along(secuencia)) {
        n <- secuencia[i]
        y <- generar(n)
        y <- y[y > 1e-3]
        x <- (1 - y) / y
        fx <- sapply(x, f)
        h_y <- fx / (y^2)
        h_y <- h_y[is.finite(h_y) & abs(h_y) < 1e6]
        aprox[i] <- mean(h_y, trim = 0.05, na.rm = TRUE)
        if (abs(aprox[i]) > 1e6) {
          es_divergente <- TRUE
          razon_divergencia <- "La aproximación crece sin límite"
          break
        }
      }
      if (!es_divergente) {
        teorico <- tryCatch({
          f0 <- f(0)
          if (is.finite(f0) && !is.nan(f0)) {
            integrate(f, lower = 0, upper = Inf)$value
          } else {
            integrate(f, lower = 1, upper = Inf)$value
          }
        }, error = function(e) NA)
      }
    }
    
    if (es_divergente) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "INTEGRAL DIVERGENTE", size = 8, color = "red") +
          annotate("text", x = 0.5, y = 0.4, label = razon_divergencia, size = 5, color = "darkred") +
          theme_void() +
          labs(caption = "La integral no converge a un valor finito")
      )
    }
    
    plot_data <- data.table(Aproximación = aprox,
                            Teórico = rep(teorico, length(secuencia))) %>%
      tidyr::pivot_longer(cols = c("Aproximación", "Teórico"),
                          names_to = "Etiqueta", values_to = "Valor") %>%
      mutate(Aleatorios = rep(secuencia, 2))
    
    ggplot(plot_data, aes(x = Aleatorios, y = Valor, color = Etiqueta)) +
      geom_line(size = 1.2) +
      geom_point(size = 1.5, alpha = 0.6, show.legend = FALSE) +
      scale_color_manual(values = c("Aproximación" = "#e74c3c", "Teórico" = "#3498db")) +
      labs(
        title = paste("Estimación del área de:", f_input),
        subtitle = if (limite_infinito) "Integral impropia en el intervalo [0, ∞)" else paste("Intervalo definido:", a, "a", b),
        x = "Muestras aleatorias generadas",
        y = "Área estimada",
        caption = if (!is.na(teorico)) paste("Valor teórico esperado:", round(teorico, 4)) else "Estimación por método Monte Carlo"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
        plot.subtitle = element_text(size = 13, color = "#7f8c8d"),
        axis.title.x = element_text(face = "bold", color = "#2c3e50"),
        axis.title.y = element_text(face = "bold", color = "#2c3e50"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11)
      )
  })
  
  ####### DISTRIBUCIONES DISCRETAS #################################################################
  
  # Binomial Distribution
  observeEvent(input$simular_binomial, {
    req(input$n_binomial, input$p_binomial, input$simulaciones_binomial)
    
    n <- input$simulaciones_binomial
    size <- input$n_binomial
    prob <- input$p_binomial
    
    resultados <- rbinom(n, size, prob)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#1e81b0", color = "white", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = resultados)
      x_range <- 0:max(max(resultados), size)
      pmf_data <- data.frame(
        x = x_range,
        Prob = dbinom(x_range, size = size, prob = prob)
      )
      
      ggplot(datos, aes(x = Valores)) +
        geom_bar(aes(y = ..prop..), fill = "#69b3a2", color = "black", width = 0.4) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "red", size = 3) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "red", size = 1) +
        labs(
          title = paste("Histograma de Simulaciones - Binomial (n =", size, ", p =", prob, ")"),
          x = "Valores Simulados",
          y = "Proporción"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
    })
    
    output$resumen_estadistico <- renderPrint({
      cat("Resumen de Simulaciones:\n")
      cat("Media:", round(mean(resultados), 4), "\n")
      cat("Desviación Estándar:", round(sd(resultados), 4), "\n")
      cat("Mínimo:", min(resultados), "\n")
      cat("Máximo:", max(resultados), "\n")
      cat("Cuartiles:\n")
      print(quantile(resultados))
    })
  })
  
  # Poisson Distribution
  observeEvent(input$simular_poisson, {
    req(input$lambda_poisson, input$simulaciones_poisson)
    
    n <- input$simulaciones_poisson
    lambda <- input$lambda_poisson
    
    resultados <- rpois(n, lambda)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#1e81b0", color = "white", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = resultados)
      x_range <- 0:max(max(resultados), qpois(0.999, lambda))
      pmf_data <- data.frame(
        x = x_range,
        Prob = dpois(x_range, lambda = lambda)
      )
      
      ggplot(datos, aes(x = Valores)) +
        geom_bar(aes(y = ..prop..), fill = "#69b3a2", color = "black", width = 0.4) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "red", size = 3) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "red", size = 1) +
        labs(
          title = paste("Histograma de Simulaciones - Poisson (λ =", lambda, ")"),
          x = "Valores Simulados",
          y = "Proporción"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
    })
    
    output$resumen_estadistico <- renderPrint({
      cat("Resumen de Simulaciones:\n")
      cat("Media:", round(mean(resultados), 4), "\n")
      cat("Desviación Estándar:", round(sd(resultados), 4), "\n")
      cat("Mínimo:", min(resultados), "\n")
      cat("Máximo:", max(resultados), "\n")
      cat("Cuartiles:\n")
      print(quantile(resultados))
    })
  })
  
  # Negative Binomial Distribution
  observeEvent(input$simular_binomial_neg, {
    req(input$r_binomial_neg, input$p_binomial_neg, input$simulaciones_binomial_neg)
    
    n <- input$simulaciones_binomial_neg
    size <- input$r_binomial_neg
    prob <- input$p_binomial_neg
    
    resultados <- rnbinom(n, size, prob)
    
    output$resultados_simulacion <- function() {
      res <- conv_matrix(resultados, cols = 10)
      kbl(res, booktabs = TRUE, escape = FALSE, align = "c", digits = 0) %>%
        kable_styling(
          full_width = FALSE,
          bootstrap_options = c("bordered", "striped", "hover", "condensed"),
          font_size = 13
        ) %>%
        row_spec(0, background = "#1e81b0", color = "white", bold = TRUE) %>%
        scroll_box(width = "100%", height = "250px")
    }
    
    output$histograma_simulacion <- renderPlot({
      datos <- data.frame(Valores = resultados)
      x_range <- 0:max(max(resultados), qnbinom(0.999, size, prob))
      pmf_data <- data.frame(
        x = x_range,
        Prob = dnbinom(x_range, size = size, prob = prob)
      )
      
      ggplot(datos, aes(x = Valores)) +
        geom_bar(aes(y = ..prop..), fill = "#69b3a2", color = "black", width = 0.4) +
        geom_point(data = pmf_data, aes(x = x, y = Prob), color = "red", size = 3) +
        geom_line(data = pmf_data, aes(x = x, y = Prob), color = "red", size = 1) +
        labs(
          title = paste("Histograma de Simulaciones - Binomial Negativa (r =", size, ", p =", prob, ")"),
          x = "Valores Simulados",
          y = "Proporción"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#2c3e50"),
          axis.title = element_text(face = "bold")
        )
    })
    
    output$resumen_estadistico <- renderPrint({
      cat("Resumen de Simulaciones:\n")
      cat("Media:", round(mean(resultados), 4), "\n")
      cat("Desviación Estándar:", round(sd(resultados), 4), "\n")
      cat("Mínimo:", min(resultados), "\n")
      cat("Máximo:", max(resultados), "\n")
      cat("Cuartiles:\n")
      print(quantile(resultados))
    })
  })
}