
###########    Trabajo 1- Simulación-CAMBIOS
###########       Casamen Jordan

###########          Ejercicio
# Para U1,U2,.... variables aleatorias uniformes en (0,1),definimos
# N=min{n:sum_{i=1}^n Ui>1}
# Es decir, N es igual a la cantidad de números aleatorios que deben sumarse para exceder a 1



# Función para calcular N 
N <- function(){
  suma <- 0
  n <- 0
  # Genero U_i uno por uno hasta que la suma pase de 1
  while(suma <= 1){
    u <- runif(1)  #  genera valores entre 0 y 1 
    suma <- suma + u
    n <- n + 1
  }
  return(n)
}

# (a) Estime E[N] generando 100 valores de N
res_a <- sapply(1:100, function(k) N())
cat("Estimación de E[N] con 100 simulaciones es:", mean(res_a))
# La estimación es de 2.69 (diferentes valores por el runif(1) en cada ejecución)

# (b) Estime E[N] generando 1000 valores de N
res_b <- sapply(1:1000, function(k) N())
cat("Estimación de E[N] con 1000 simulaciones es:", mean(res_b))
#La estimación es de 2.704 (diferentes valores por el runif(1) en cada ejecución)

# (c) Estime E[N] generando 10000 valores de N
res_c <- sapply(1:10000, function(k) N())
cat("Estimación de E[N] con 10000 simulaciones es:", mean(res_c))
#La estimación es de 2.7156 (diferentes valores por el runif(1) en cada ejecución)

# (d) ¿Cuál cree que sea el valor de E[N]?
print("Podemos observar que mientras más simulaciones hagamos el valor se aproxima a 2.71 que es la constante de euler ")


# Gráfico estimaciones de E[N]
#valores_n <- c(100, 1000, 10000)
#estimaciones <- c(mean(res_a), mean(res_b), mean(res_c))

#plot(valores_n, estimaciones, type = "o", col = "green",
 #    xlab = "Número de simulaciones", ylab = "Promedio de N",
  #   main = "Estimación de E[N]")
#abline(h = exp(1), col = "red", lty = 2)
