################################################################################
#                              EJERCICIO 3.1
# Crea una función denominada puntuacion que simule tirar dos dados, y devuelva
# la suma. Los lanzamientos son independientes.
# Nota: utiliza la función sample. Deben poder obtenerse valores como 2 o 12
################################################################################
puntuacion <- function() {
    suma = sample(1:6, 1) + sample(1:6, 1)
    suma
}

################################################################################
#                              EJERCICIO 3.2
# Llamar 10 veces a la funcion puntuacion
################################################################################
print("EJERCICIO 3.2")
for (i in 1:10) {print(puntuacion())}

################################################################################
#                              EJERCICIO 3.3
# Rellenar un vector con 100 puntuaciones. Versión 1. Agregando las salidas de 
# 2.3 a un vector. Versión 2. Hacer una función que devuelva un vector de 
# longitud varible. 
################################################################################
print("EJERCICIO 3.3")
print("Versión 1")
vec = vector()
for (i in 1:100) {
    vec <- append(vec, puntuacion())
}
print(vec)

print("Versión 2")
long_var <- function(n=1) {
    vect = vector(length = n)
    for (i in 1:n) {
        vect[i] <- puntuacion()
    }
    vect
} 
print(long_var(26))

################################################################################
#                              EJERCICIO 3.4
# Vamos a manipular una baraja española, a barajarla y a robar de la baraja.
# Para ello, será necesario crear la bajara: un dataframe y dos funciones, 
# barajar y robar.
################################################################################
print("EJERCICIO 3.4")
palos <- c("espadas","bastos","oros","copas")
cartas <- c("as","dos","tres","cuatro","cinco","seis","siete","ocho","nueve","sota","caballo","rey")
valores <- 1:12
baraja <- data.frame(palos,cartas=rep(cartas,each=4),valores=rep(valores,each=4))
print(baraja)

barajar <- function() {
    barajado <- baraja[sample(nrow(baraja)),]
    barajado
}
print("Baraja barajada")
baraja <- barajar()
print(baraja)

robar <- function() {
    robada <- baraja[1,1:3,drop=FALSE]
    robada
}
print("Carta robada")
robada <- robar()
print(robada)
baraja <- baraja[-1,]       # quitamos la carta del mazo
print("Baraja sin la carta robada")
print(baraja)
