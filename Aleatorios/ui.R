library(shiny)
library(kableExtra)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Generación de números Aleatorios"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          numericInput("semilla", "Ingrese un valor Inicial", value = 3, min = 0, max = 100),
          numericInput("divisor", "Ingrese el valor de m", value = 5, min = 0, max = 100),
          numericInput("constante", "Ingrese el valor de a", value = 7, min = 0, max = 100),
          numericInput("num", "Cantidad de numeros aleatorios", value = 10, min = 0, max = 100),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("tabla")
        )
    )
)
