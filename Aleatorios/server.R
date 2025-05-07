library(shiny)
library(kableExtra)

# Función que genera números aleatorios bajo el método congruencial
random_cong <- function(a, m, x0, n) {
  res <- numeric(n)  # crea un vector nulo de n elementos
  res[1] <- x0
  for(k in 2:n) {
    res[k] <- (a * res[k-1]) %% m
  }
  return(res[-1])
}

# Llamada a la función
random_cong(a=5, x0=3, m=7, n=12)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$tabla <- function(){
      res<<-data.table(Valor=random_cong(a=input$constante,m=input$divisor,x0=input$semilla,n=input$num))
      res[, n:=1: nrow(res)]
      res <- res[,c("n","Valor"),with=FALSE]
      
      #Formato de la tabla
      kbl(res,booktabs= TRUE,escape = FALSE) %>% 
        kable_styling(full_width = FALSE,bootstrap_options = c("bordered"),font_size = 12) %>%
        row_spec(0,background = "#1D3889",color = "#ffffff")
    }

}
