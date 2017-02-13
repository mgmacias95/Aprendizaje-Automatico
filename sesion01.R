################################################################################
#                           EJERCICIO 2.2
# Crear una secuencia de 1 a 1000 en 4 intervalos
################################################################################
# Como 1000/4=250, la secuencia será
print("EJERCICIO 2.2")
x <- seq(1, 1000, length.out=4)
print(x)

################################################################################
#                           EJERCICIO 2.3
# Cree un vector de 10 componentes enteras decrecientes denominado s. Compruebe
# los atributos que tiene el sistema sobre los tipos de vectores x y s
################################################################################
print("EJERCICIO 2.3")
x <- c(1.1, 2.2, 3.3, 4.4)
print(x)
y <- x+10
print(y)
xx <- c(x,0,x)
print(xx)
s <- 10L:1L    # Vector de 10 valores enteros
print(s)
print(length(x))      # Longitud de x
print(length(s))      # Longitud de s
print(mode(x))        # Modo de x
print(mode(s))        # Modo de s
print(class(x))       # Clase de x
print(class(s))       # Clase de s

################################################################################
#                           EJERCICIO 2.4
# Estima las longitudes resultantes de las operaciones siguientes:
#       - x + 1 + 2 * y
#       - x - y
#       - x + s
#       - xx + x
################################################################################
print("EJERCICIO 2.4")
print(length(x + 1 + 2 * y))
print(length(x - y))
print(length(x + s))
print(length(xx + x))

################################################################################
#                           EJERCICIO 2.5
# Deje ordenado x en orden decreciente
################################################################################
print("EJERCICIO 2.5")
print(sort(x, decreasing=TRUE))

################################################################################
#                           EJERCICIO 2.6
# Calcule la media del vector x, usando sum() y length()
################################################################################
print("EJERCICIO 2.6")
print(sum(x)/length(x))

################################################################################
#                           EJERCICIO 2.7
# Se registran ventas de 5 artículos el sábado, se registran en otro vector las
# ventas del domingo. Guarde en un vector articulo.nombres los nombres de los
# articulos correspondientes a las ventas. Obtener en un vector finde.ventas las
# ventas del fin de semana.
################################################################################
print("EJERCICIO 2.7")
sabado.ventas = c(5L, 4L, 10L, 3L, 4L)
domingo.ventas = c(7L, 5L, 9L, 4L, 5L)
nombres.ventas = c("mermelada", "chocolate", "café", "azúcar", "galletas")
ventas <- data.frame(nombres.ventas, sabado.ventas, domingo.ventas)
print(ventas)

################################################################################
#                           EJERCICIO 2.8
# Simule el lanzamiento de dos dados. Los lanzamientos son independientes.
# Nota: utilice la funcion sample, necesitará algún parámetro, ¿cuál?
################################################################################
print("EJERCICIO 2.8")
print(sample(1:6, 1))
print(sample(1:6, 1))

################################################################################
#                           EJERCICIO 2.9
# Dado un vector de puntuaciones se han de quitar los valores extremos
################################################################################
print("EJERCICIO 2.9")
punt <- sample(1:10)
print(punt[punt < max(punt) & punt > min(punt)])

################################################################################
#                           EJERCICIO 2.10
# Dado el vector actual de w, ignore los valores perdidos
################################################################################
print("EJERCICIO 2.10")
w <- numeric()             # crea el objeto y reserva espacio     
length(w) <- 10; print(w)  # se redimensiona y se muestra el contenido
w[3:5] <- 5; print(w)
print(w[complete.cases(w)])

################################################################################
#                           EJERCICIO 2.11
# Elimine todos los objetos creados durante la sesión
################################################################################
print("EJERCICIO 2.11")
print(ls())
rm(list = ls())
print(ls())
