################################################################################
#                              EJERCICIO 4.1
# Representar en un diagrama de quesos, pie, la misma información que la que se
# muestra en el siguiente comando.
#       > hist(x, main="Histograma", breaks=10)
# Nota: se puede guardar los cálculos del histograma sin mostrarlo en una variable
# con el parámetro plot=F
################################################################################
print("EJERCICIO 4.1")
x <- rnorm(100) # datos generales a partir de una normal
lista <- hist(x, breaks=10, plot=F) # guardamos los datos de la gráfica en una lista
# representamos en un diagrama de quesos el número de veces que cada "break" aparece
# etiquetando cada uno con su respectiva zona del histograma
pie(x=lista$counts, labels=lista$breaks)

################################################################################
#                              EJERCICIO 4.2
# Cambie el valor de la esquina superior izquierda a -1 y los valores de la tercera
# fila a 1 de m
################################################################################
print("EJERCICIO 4.2")
m <- matrix(1:8, nrow=2, ncol=4, byrow=TRUE)
print(m)
m[1,4] = -1
m[2,]  = 1
print(m)

################################################################################
#                              EJERCICIO 4.3
# Asigne el valor -2 a la diagonal principal de la matriz a
################################################################################
print("EJERCICIO 4.3")
a <- array(1:5, dim=c(5,5))     # genera array de 5x5 por columnas
print(a)
i <- array(c(1:5, 1:5), dim=c(5,2)) # genera array de indices 1,1;2,2 etc
a[i] = -2  # establece para cada coordenada de i el valor de -2
print(a)
