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
  
  # Filtro de variables por su tipo
  tvar <- datos |> map_chr(class) 
  tvar <- data.table(Variable = names(tvar), Tipo = unname(tvar))
  
  observe({
    # Actualizar las opciones del selectInput según el tipo seleccionado
    choices <- tvar[Tipo == input$tipo]$Variable
    updateSelectInput(session, "var", choices = choices, selected = choices[1])
  })
  
  output$resumen <- renderPrint({
    datos %>% dplyr::select(input$var) %>% summary(.)
  })
  
  output$grafico <- renderPlot({
    if(input$tipo == "numeric"){
      datos %>% dplyr::select(input$var) %>% pull(.) %>% hist(.)
    } else {
      datos %>% dplyr::select(input$var) %>% table(.) %>% barplot(.)
    }
  })
}