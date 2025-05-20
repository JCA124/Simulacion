library(shiny)
library(kableExtra)
library(shinycssloaders)

fluidPage(
  titlePanel("Generación de números aleatorios e integrales"),
  
  tabsetPanel(
    tabPanel("Números Aleatorios",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("semilla", "Valor inicial:", min = 1, max = 500, value = 30),
                 sliderInput("divisor", "Valor de m:", min = 1, max = 500, value = 37),
                 sliderInput("constante", "Valor de a:", min = 1, max = 500, value = 123),
                 sliderInput("incremento", "Valor de c (método mixto):", min = 0, max = 500, value = 1),
                 sliderInput("cantidad", "Cantidad de números:", min = 1, max = 200, value = 24),
                 numericInput("filas", "Filas de la tabla:", value = 3, min = 1, max = 50),
                 numericInput("columnas", "Columnas de la tabla:", value = 10, min = 1, max = 50),
                 radioButtons("metodo", "Método:", 
                              choices = c("Multiplicativo", "Mixto", "Ambos"),
                              selected = "Ambos"),
                 actionButton("mostrar", "Mostrar resultados", icon("eye"))
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.mostrar != 0 && (input.metodo == 'Multiplicativo' || input.metodo == 'Ambos')",
                   h4("Tabla - Método Multiplicativo"),
                   withSpinner(tableOutput("tabla_multiplicativo"))
                 ),
                 conditionalPanel(
                   condition = "input.mostrar != 0 && (input.metodo == 'Mixto' || input.metodo == 'Ambos')",
                   h4("Tabla - Método Mixto"),
                   withSpinner(tableOutput("tabla_mixto"))
                 ),
                 conditionalPanel(
                   condition = "input.mostrar != 0",
                   h4("Histogramas"),
                   fluidRow(
                     column(width = 6,
                            h5("Método Multiplicativo"),
                            withSpinner(plotOutput("hist_multiplicativo"))),
                     column(width = 6,
                            h5("Método Mixto"),
                            withSpinner(plotOutput("hist_mixto")))
                   )
                 )
               )
             )
    ),
    tabPanel("Integrales",
             sidebarLayout(
               sidebarPanel(
                 textInput("funcion", "Función a integrar (en x):", value = "1-x"),
                 textInput("lim_inf", "Límite inferior (número o -Inf):", value = "0"),
                 actionButton("inf_neg", "−∞", icon("infinity")),
                 textInput("lim_sup", "Límite superior (número o Inf):", value = "1"),
                 actionButton("inf_pos", "∞", icon("infinity")),
                 radioButtons("metodo_int", "Método para números aleatorios:", 
                              c("Multiplicativo", "Mixto")),
                 actionButton("calcular", "Calcular área", style = "color: #FFFFFF; background-color: #F5426C; border-color: #9932CC")
               ),
               mainPanel(
                 conditionalPanel(condition = "input.calcular != 0",
                                  h4("Gráfica del área bajo la curva"),
                                  withSpinner(plotOutput("graf_area")),
                                  h4("Aproximación de Monte Carlo"),
                                  withSpinner(plotOutput("graf_aprox"))
                 )
               )
             )
    )
  )
)