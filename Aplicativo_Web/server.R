library(shiny)
library(tidyverse)
library(lubridate)
library(data.table)

# Carga de datos
load("/cloud/project/Aplicativo_Web/data/Info.RData")

# Ajuste de formato fecha
datos[, fecha_corte := ymd(fecha_corte)]
datos[, fecha_nacimiento := ymd(fecha_nacimiento)]

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Definir tvar como un objeto reactivo
  tvar <- reactive({
    req(datos) # Asegurarse de que datos existe
    classes <- tryCatch({
      map_chr(datos, class)
    }, error = function(e) {
      message("Error al obtener clases de columnas: ", e$message)
      character(0) # Retornar vector vacío si hay error
    })
    if (length(classes) == 0) {
      data.table(Variable = character(), Tipo = character())
    } else {
      data.table(Variable = names(classes), Tipo = unname(classes))
    }
  })
  
  observe({
    # Asegurarse de que tvar() esté disponible
    req(tvar())
    # Actualizar las opciones del selectInput según el tipo seleccionado
    choices <- tvar()[Tipo == input$tipo]$Variable
    if (length(choices) > 0) {
      updateSelectInput(session, "var", choices = choices, selected = choices[1])
    } else {
      updateSelectInput(session, "var", choices = NULL, selected = NULL)
    }
  })
  
  # Mensaje cuando no hay variables disponibles
  output$mensaje_vars <- renderUI({
    req(tvar())
    choices <- tvar()[Tipo == input$tipo]$Variable
    if (length(choices) == 0) {
      tags$p(style = "color: red;", "No hay variables disponibles para este tipo.")
    } else {
      NULL
    }
  })
  
  output$resumen <- renderPrint({
    # Validar que input$var no sea NULL ni vacío
    req(input$var, input$var != "")
    datos %>% dplyr::select(input$var) %>% summary(.)
  })
  
  output$grafico <- renderPlot({
    # Validar que input$var no sea NULL ni vacío
    req(input$var, input$var != "")
    if (input$tipo == "numeric") {
      datos %>% dplyr::select(input$var) %>% pull(.) %>% hist(.)
    } else {
      datos %>% dplyr::select(input$var) %>% table(.) %>% barplot(.)
    }
  })
}
  