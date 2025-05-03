library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  tags$style("h1 {color: #d8cdaa; font-size:35px}"),
  tags$style("h2 {color: #b7b9c1; font-size:25px}"),
  fluidRow(column(width = 3, tags$img(src="EPN_logo.png", width = "60px", height = "60px")),
           column(width = 9, h1("Primer aplicativo Simulación", style = "text-align:center"))),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      #Escogemos el tipo de variables
      radioButtons("tipo", "Seleccione el tipo de variables a analizar:", 
                   choiceNames = c("Cuantitativas", "Cualitativas"),
                   choiceValues = c("numeric", "character"), selected = "numeric"),
      
      #Escogemos la Variable
      selectInput("var", "Variable seleccionada",
                  choices = tvar$Variable, selected = tvar$Variable[2]),
      
      #Escogemos los colores para el Histograma
        #Color de Barras del Histograma
      radioButtons("Color_barras","Seleccione el Color de las barras del Histograma",
                   choices = c("Rosado"="#ffa2c5","Rojo Ladrillo"="#9a0c44","Beige"="#fdf2e2",
                               "Anaranjado"="#fe9600","Mostaza"="#dcd15c","Verde Oliva"="#badc80",
                               "Turqueza"="#80dccf","Azul Marino"="#1b6c85","Lila"="#e68bff",
                               "Sky Blue"="#bad5f5"),selected ="#badc80" ),
      
        #Color de Bordes del Histograma
      radioButtons("Color_bordes","Seleccione el Color de las bordes del Histograma",
                   choices = c("Vino"="#7e2c2c","Morado Oscuro"="#951058","Verde Oscuro"="#066d29",
                               "Rosado Oscuro"="#dd6f6f"),selected = "#951058"),
      
        #Tamaño del Histograma
      radioButtons("size_hist", "Tamaño del gráfico",
                   choices = c("Grande" = "500px", "Mediano" = "400px", "Pequeño" = "300px"),
                   selected = "400px")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Resumen:"),
      verbatimTextOutput("resumen"),
      h2("Gráfico"),
      uiOutput("grafico_dinamico")
    )
  )
)
