################################################################################
#                     PRÁCTICA 1 APRENDIZAJE AUTOMÁTICO
#                            Marta Gómez Macías
#                                 Grupo 1
#                           1 de Marzo de 2016
################################################################################

#               Ejercicio de Generación y Visualización de datos               #

# 1. Construir una función lista = simula_unif(N,dim,rango) que calcule una lista
# de longitud N de vectores de dimensión dim conteniendo números aleatorios
# uniformes en el intervalo rango
lista = simula_unif <- function(N=5, dim=20, rango=1:50) {
    # devolvemos una matriz con N filas y dim columnas, cada una rellenada con 
    # valores producidos por runif.
    matrix(runif(dim*N, min=rango[1], max=tail(rango,1)), ncol=dim, nrow=N, byrow=T) 
}

# 2. Construir una función lista = simula_gaus(N,dim,sigma) que calcule una lista
# de longitud N de vectores de dimensión dim conteniendo números aleatorios
# gaussianos de media 0 y varianzas dadas por el vector sigma
lista = simula_gaus <- function(N=5, dim=20, sigma=0.5:5.0) {
    # devolvemos una matriz en cuyas filas hay valores generados por rnorm
    matrix(rnorm(dim*N, 0, sigma), ncol=dim, nrow=N, byrow=T)
}

# 3. Suponer N = 50, dim = 2, rango = [-50, +50] en cada dimensión. Dibujar una 
# gráfica de la salida de la función correspondiente.
representa_unif <- function(N=50, dim=2, rango=-50:50) {
    # inicializamos la matriz de puntos a representar
    x = simula_unif(N, dim, rango)
    # inicializamos los limites de la grafica
    plot(rango, rango, type="n", xlab="", ylab="", main="Ejercicio 1.3")         
    # representamos la matriz de puntos
    points(x=x, col=colors(), lwd=2, pch=19)
}

# 4. Suponer N = 50, dim = 2 y sigma = [5,7]. Dibujar una gráfica de la salida
# de la función correspondiente.
representa_gaus <- function(N=50, dim=2, sigma=5:7) {
    # inicializamos la matriz de puntos a representar
    x = simula_gaus(N, dim, sigma)
    # inicializamos los límites de la gráfica
    plot(range(x[,1]), range(x[,2]), type="n", xlab="", ylab="",
        main="Ejercicio 1.4")
    points(x=x, col=colors(), lwd=2, pch=19)
}

# 5. Construir la función v = simula_recta (intervalo) que calcula los parámetros,
# v = (a,b) de una recta aleatoria, y = ax + b, que corte al cuadrado [-50,50]x[50,50].
# (Ayuda: Para calcular la recta simular las coordenadas de dos puntos dentro del
# cuadrado y calcular la recta que pasa por ellos).
calcula_recta <- function(p1x, p1y, p2x, p2y) {
    # calculamos la ecuación de la recta, para ello, lo hacemos como el siguiente
    # ejemplo: A(1,3) y B(2,-5) (http://www.vitutor.com/geo/rec/d_7.html)
    #       x - 1     y - 3
    #      ------- = -------- 
    #       2 - 1     -5 - 3
    a = (p2y - p1y)/(p2x - p1x)
    b = (((p2y - p1y) * -p1x) - ((p2x - p1x) * 
        -p1y))/(p2x - p1x)
    # cat(sprintf("y = %fx + %f\n", a, b))
    c(a,b)      # devolvemos los valores a y b calculados
}

v = simula_recta <- function(intervalo=-50:50) {
    # calculamos dos puntos aleatorios dentro del intervalo
    puntos = sample(intervalo, 4)
    # comento tanto el print puntos como el cat para que no se impriman al 
    # ejecutar el script
    # print(puntos)
    # devolvemos los parámetros a y b calculados
    calcula_recta(puntos[1], puntos[2], puntos[3], puntos[4])
}

# 6. Generar una muestra 2D de puntos usando simula_unif() y etiquetar la muestra
# usando el signo de la función f(x,y) = y - ax - b de cada punto a una recta
# simulada con simula_recta(). Mostrar una gráfica con el resultado de la muestra
# etiquetada junto con la recta usada para ello.

# obtenemos una lista de puntos aleatorios
p = simula_unif(N=1000, dim=2, rango=-50:50)
# generamos una recta aleatoria en el intervalo por defecto
recta = simula_recta()

# definimos la función a aplicar a cada punto
funcion_etiquetado <- function(vec,rec=recta) aux = sign(vec[2] - rec[1]*vec[1] - rec[2])

obten_param_recta <- function(puntos=p,re=recta) {
    # para cada punto, calculamos su valor para la función dada
    # y lo etiquetamos con su respectivo valor. Para ello, usamos una funcion
    # que decida si un numero es positivo o negativo
    apply(X=puntos, FUN=funcion_etiquetado, MARGIN=1, rec=re)
}

representa_recta <- function(puntos=p, re=recta, nombres, titulo) {
    # calculamos los límites de la gráfica
    lim_x = range(puntos[,1])
    lim_y = range(puntos[,2])

    # representamos la recta
    plot(lim_x, lim_y, type="n", xlab="x", ylab="y = ax + b", main=titulo)
    abline(a=re[2], b=re[1])
    # lines(x=c(lim_x[1], lim_x[2]), y=c(re[1]*lim_x[1]+re[2], re[1]*lim_x[2]+re[2]))        
    # representamos cada punto calculado con simula_unif del color correspondiente
    # a su clasificación
    points(x=puntos, col=colors()[138+nombres], lwd=2, pch=19)
}

ejercicio_seis <- function(titulo, puntos=p) {
    # obtenemos el etiquetado de los puntos según la recta
    etiquetas = obten_param_recta(puntos,recta)
    # representamos la recta
    representa_recta(puntos, recta, etiquetas,titulo)
}

# 7. Usar la muestra generada en el apartado anterior y etiquetarla con +1,-1 
# usando el signo de cada una de las siguientes funciones:
#   * f (x,y) = (x - 10)^2 + (y - 20)^2 - 400
#   * f (x,y) = 0.5(x + 10)^2 + (y - 20)^2 - 400
#   * f (x,y) = 0.5(x - 10)^2 - (y + 20)^2 - 400
#   * f (x,y) = y - 20x^2 - 5x + 3
# Visualizar el resultado del etiquetado de cada función junto con su gráfica y
# comparar el resultado con el caso lineal. ¿Qué consecuencias extrae sobre la 
# forma de las regiones positiva y negativa?
# definimos un vector con las funciones a evaluar con modo vectorial
funciones = c(function(vec) sign((vec[1] - 10)*(vec[1] - 10) + (vec[2] - 20)*(vec[2] - 20) - 400),
            function(vec) sign(0.5*((vec[1] + 10)*(vec[1] + 10)) + (vec[2] - 20)*(vec[2] - 20) - 400),
            function(vec) sign(0.5*((vec[1] - 10)*(vec[1] - 10)) - (vec[2] - 20)*(vec[2] - 20) - 400),
            function(vec) sign(vec[2] - 20*vec[1]^2 - 5*vec[1] + 3))
# y otro con modo x,y para representar las funciones
funciones_xy = c(function(x,y) (x - 10)*(x - 10) + (y - 20)*(y - 20) - 400,
                function(x,y) 0.5*((x + 10)*(x + 10)) + (y - 20)*(y - 20) - 400,
                function(x,y) 0.5*((x - 10)*(x - 10)) - (y - 20)*(y - 20) - 400,
                function(x,y) y - 20*x^2 - 5*x + 3)

dibuja_funcion <- function(puntos=p, funcion, nombres, titulo) {
    x <- y <- seq(range(puntos[,1])[1],range(puntos[,1])[2],length=100) # guardamos el intervalo a representar
    z <- outer(x,y,funcion)             # calculamos los puntos de la funcion
    contour (                           # la representamos
        x=x, y=x, z=z,
        levels=0, las=1, drawlabels=FALSE, main=titulo
    )
    # representamos los puntos
    points(x=puntos, col=colors()[138+nombres], lwd=2, pch=19)
}

# funcion que calcula el etiquetado de cada punto según la función que pasemos 
# como parámetro
calcula_nombres <- function(funcion, puntos) apply(X=puntos, FUN=funcion, MARGIN=1)
etiqueta_puntos <- function(puntos=p) lapply(funciones, calcula_nombres, puntos=puntos)

ejercicio_siete <- function() {
    # calculamos el etiquetado de los puntos según cada una
    nombres <- etiqueta_puntos()

    # representamos cada función
    for (i in 1:4) {
        dibuja_funcion(funcion=funciones_xy[[i]], nombres=nombres[[i]], titulo=paste("Ejercicio 7.",i, sep=""))
        print("Pulsa s para ejecutar el siguiente ejercicio...")
        scan(what=character(), n=1)
    }
}

# 8. Considerar de nuevo la muestra etiquetada en el apartado 6. Modifique las
# etiquetas de un 10% aleatorio de muestras positivas y otro 10% aleatorio de 
# negativas.
#   * Visualice los puntos con las nuevas etiquetas y la recta del apartado 6.
#   * En una gráfica aparte visualice de nuevo los mismos puntos pero junto con
#     las funciones del apartado 7.
# Observe las gráficas y diga qué consecuencias extrae del proceso de 
# modificación de etiquetas en el proceso de aprendizaje.
# función para obtener datos con ruido
mete_ruido <- function(nombres=list(etiqueta_puntos()[[1]], etiqueta_puntos()[[2]],
        etiqueta_puntos()[[3]], etiqueta_puntos()[[4]], obten_param_recta())) {
    # obtenemos los puntos etiquetados como positivos
    positivos = lapply(nombres, function(nom) which(nom %in% 1))

    # y los etiquetados como negativos
    negativos = lapply(nombres, function(nom) which(nom %in% -1))

    # obtenemos indices aleatorios de un 10% de las muestras
    valores_aleatorios <- function (vec) sample(vec, 0.1*length(vec))
    ale_pos = lapply(positivos, valores_aleatorios)
    ale_neg = lapply(negativos, valores_aleatorios)

    # cambiamos el valor de los indices aleatorios obtenidos
    for(i in 1:length(nombres)) {
        nombres[[i]][ale_pos[[i]]] = -1
        nombres[[i]][ale_neg[[i]]] = +1
    }
    nombres
}

ejercicio_ocho <- function() {
    # etiquetamos los puntos del ejercicio 7
    nombres = etiqueta_puntos()
    # añadimos el etiquetado del ejercicio 6 al del ejercicio 7
    nombres = list(etiqueta_puntos()[[1]], etiqueta_puntos()[[2]],
        etiqueta_puntos()[[3]], etiqueta_puntos()[[4]], obten_param_recta())

    # calculamos el etiquetado de la funcion con ruido
    nombres = mete_ruido(nombres=nombres)

    # representamos la recta
    representa_recta(nombres=nombres[[5]], titulo="Ejercicio 8.1")
    
    # representamos las funciones
    for(i in 1:4) {
        print("Pulsa s para ejecutar el siguiente ejercicio...")
        scan(what=character(), n=1)
        dibuja_funcion(funcion=funciones_xy[[i]], nombres=nombres[[i]], 
            titulo=paste("Ejercicio 8.",i+1,sep=""))
    }
}

# ejecución del script
# print("Práctica 1 de Aprendizaje Automático")
# print("Marta Gómez Macías")
# print("Ejercicio de Generación y Visualización de datos")
# print("Apartado 1")
# print(simula_unif())
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 2")
# print(simula_gaus())

# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 3")
# representa_unif()
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 4")
# representa_gaus()
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 5")
# r = simula_recta()
# cat(sprintf("y = %fx + %f\n", r[1], r[2]))
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 6")
# ejercicio_seis(titulo="Ejercicio 6")
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 7")
# ejercicio_siete()

# print("Apartado 8")
# ejercicio_ocho()

#               Ejercicio de Ajuste del Algoritmo Perceptrón                   #

# 1. Implementar la función sol = ajusta_PLA(datos,label,max_iter,vini) que 
# calcula el hiperplano solución a un problema de clasificación binaria usando
# el algoritmo PLA. La entrada datos es una matriz donde cada item con su 
# etiqueta está representado por una fila de la matriz, label el vector de 
# etiquetas (cada etiqueta es un valor +1 o -1), max_iter es el número máximo de
# iteraciones permitidas y vini el valor inicial del vector. La salida sol
# devuelve los coeficientes del hiperplano.
sol = ajusta_PLA <- function(datos,label, max_iter=1000, vini=c(0,0,0)) {
    cambiado = T
    i = 1        # variable para contar el número de iteraciones
    # añadimos a la matriz datos una tercera columna para poder hacer el producto vectorial
    datos = cbind(rep(1, nrow(datos)), datos)
    # hacemos un bucle iterando hasta max_iter o hasta que deje de cambiar el perceptron
    while (i <= max_iter & cambiado) {
        cambiado = F
        # y hacemos un bucle interno iterando sobre cada dato
        for (j in 1:nrow(datos)) {
            # si el signo es cero, lo convertimos a uno
            signo = sign(datos[j,] %*% vini)
            if (signo == 0) {
                signo = 1
            }
            # comparamos el signo obtenido con sign con el signo que nos ha dado el
            # experto (etiqueta)
            if (label[j] != signo) {
                # si no coinciden, actualizamos el peso: wnew = wold + x*etiqueta
                vini = vini + datos[j,]*label[j]
                cambiado = T # actualizamos el valor de cambiado.
            }
        }
        i = 1+i # incrementamos el indice
    }
    # parámetros a y b del hiperplano del perceptrón y num iteraciones
    c((-vini[1]/vini[3]), (-vini[2]/vini[3]), i) 
}

# 2. Ejecutar el algoritmo PLA con los valores simulados en el apartado 6 del 
# ejercicio anterior inicializando el algoritmo con el vector cero y con vectores
# de números aleatorios en [0,1] (10 veces). Anotar el número medio de iteraciones
# necesarias en ambos para converger. Valorar el resultado.
PLA_recta <- function() {
    # inicializando el vector de pesos a cero
    # representamos cada punto calculado con simula_unif del color correspondiente
    ejercicio_seis(titulo="Ejercicio 2")
    # calculamos el perceptrón para los datos del ejercicio 6
    perceptron = ajusta_PLA(datos=p, label=obten_param_recta())

    # representamos la recta hecha por el perceptron
    abline(a=perceptron[1], b=perceptron[2], col="red",lwd=2)
    cat(perceptron[3],"iteraciones para converger")
    # inicializando el vector de pesos con valores aleatorios
    iter = vector(length=10) # creamos un vector donde guardar el num iteraciones
    for (i in 1:10) {
        # print("Pulsa s para ejecutar la siguiente gráfica...")
        # scan(what=character(), n=1)
        iter[i] = ajusta_PLA(datos=p, label=obten_param_recta(),
            vini=runif(3, min=0, max=10))[3]
        # ejercicio_seis()
        # abline(a=perceptron[1], b=perceptron[2], col="red",lwd=2)

    }
    # devolvemos el número medio de iteraciones que han hecho falta
    mean(iter)
}

# 3. Ejecutar el algoritmo PLA con los datos generados en el apartado 8 del
# ejercicio anterior usando valores de 10, 100 y 1000 para max_iter. Etiquetar los
# datos de la muestra usando la función solución encontrada y contar el número de 
# errores respecto de las etiquetas originales. Valorar el resultado.
PLA_curva <- function() {
    # obtenemos el etiquetado del ejercicio 8
    nombres = mete_ruido()

    # llamamos a la función con cada una de las funciones
    for (j in 1:3) {
        # con recta
        representa_recta(puntos=p, re=recta, nombres=nombres[[5]], 
            titulo="Ejercicio 3.1")
        # calculamos el perceptrón para los datos del ejercicio 8
        perceptron = ajusta_PLA(datos=p, label=nombres[[5]], max_iter=10^j)
        cat(perceptron[3],"iteraciones para converger")
        # representamos la recta hecha por el perceptron
        abline(a=perceptron[1], b=perceptron[2], col="red",lwd=2)

        # con curvas
        for (i in 1:length(funciones_xy)) {
            print("Pulsa s para ejecutar el siguiente ejercicio...")
            scan(what=character(), n=1)
            dibuja_funcion(funcion=funciones_xy[[i]], nombres=nombres[[i]], 
                titulo=paste("Ejercicio 3.",i+1,sep=""))
            perceptron = ajusta_PLA(datos=p, label=nombres[[i]], max_iter=10^j)
            cat(perceptron[3],"iteraciones para converger")
            abline(a=perceptron[1], b=perceptron[2], col="red",lwd=2)
        }
    }
}

# 4. Repetir el análisis del punto anterior usando la primera función del apartado 
# 7 del ejercicio anterior.
PLA_elipse <- function() {
    nombres = etiqueta_puntos()[[1]]

    dibuja_funcion(funcion=funciones_xy[[1]], nombres=nombres, titulo="Ejercicio 4")
    perceptron = ajusta_PLA(datos=p, label=nombres)
    abline(a=perceptron[1], b=perceptron[2], col="red",lwd=2)    
}

# 5. Modifique la función ajusta_PLA para que le permita visualizar los datos y
# soluciones que va encontrando a lo largo de las iteraciones. Ejecute con la 
# nueva versión el apartado 3 del ejercicio anterior.
punt_3 = simula_unif(N=50, dim=2, rango=-50:50)

ajusta_PLA_prima <- function(datos,label, max_iter=1000, vini=c(0,0,0), recta,
    puntos) {
    cambiado = T
    i = 1        # variable para contar el número de iteraciones
    # añadimos a la matriz datos una tercera columna para poder hacer el producto vectorial
    datos = cbind(rep(1, nrow(datos)), datos)
    # representamos los puntos y la recta
    ejercicio_seis(titulo="Ejercicio 5",puntos=punt_3)
    # hacemos un bucle iterando hasta max_iter o hasta que deje de cambiar el perceptron
    while (i <= max_iter & cambiado) {
        cambiado = F
        # y hacemos un bucle interno iterando sobre cada dato
        for (j in 1:nrow(datos)) {
            # si el signo es cero, lo convertimos a uno
            signo = sign(datos[j,] %*% vini)
            if (signo == 0) {
                signo = 1
            }
            # comparamos el signo obtenido con sign con el signo que nos ha dado el
            # experto (etiqueta)
            if (label[j] != signo) {
                # si no coinciden, actualizamos el peso: wnew = wold + x*etiqueta
                vini = vini + datos[j,]*label[j]
                cambiado = T # actualizamos el valor de cambiado.
                # representamos la recta
                abline(a=(-vini[1]/vini[3]), b=(-vini[2]/vini[3]),col="red",lwd=2)
                Sys.sleep(0.01)
            }
        }
        i = 1+i # incrementamos el indice
    }
    # representamos la última recta con color azul
    abline(a=(-vini[1]/vini[3]), b=(-vini[2]/vini[3]),col="blue",lwd=2)
}

ej_cinco <- function () {
    ajusta_PLA_prima(datos=punt_3, label=obten_param_recta(puntos=punt_3), recta=recta, puntos=punt_3)
}

# 6. A la vista de la conducta de las soluciones observada en el apartado anterior,
# proponga e implemente una modificación de la función original sol = ajusta_PLA_MOD(...)
# que permita obtener soluciones razonables sobre datos no linealmente separables.
# Mostrar y valorar el resultado encontrado usando los datos del apartado 7 del
# ejercicio anterior
evaluar <- function(datos, label, pesos) {
    # obtenemos el valor de la función para cada punto
    signos = apply(X=datos, FUN=function(fila) (pesos%*%fila - label)*(pesos%*%fila - label),
        MARGIN=1)
    # devolvemos la sumatoria entre el número de elementos
    mean(signos)
}

# Debemos implementar el algoritmo PLA pocket
sol = ajusta_PLA_MOD <- function(datos,label, max_iter=1000, vini=c(0,0,0)) {
    mejor_vini = vini
    cambiado = T
    i = 1        # variable para contar el número de iteraciones
    # añadimos a la matriz datos una tercera columna para poder hacer el producto vectorial
    datos = cbind(rep(1, nrow(datos)), datos)
    mejor_eval = evaluar(pesos=mejor_vini, datos=datos, label=label)
    # hacemos un bucle iterando hasta max_iter o hasta que deje de cambiar el perceptron
    while (i <= max_iter & cambiado) {
        cambiado = F
        # y hacemos un bucle interno iterando sobre cada dato
        for (j in 1:nrow(datos)) {
            # si el signo es cero, lo convertimos a uno
            signo = sign(datos[j,] %*% mejor_vini)
            if (signo == 0) {
                signo = 1
            }
            # comparamos el signo obtenido con sign con el signo que nos ha dado el
            # experto (etiqueta)
            if (label[j] != signo) {
                # si no coinciden, actualizamos el peso: wnew = wold + x*etiqueta
                vini = vini + datos[j,]*label[j]
                cambiado = T # actualizamos el valor de cambiado.
                # vemos si el nuevo vini es mejor que el mejor global
            }
        }
        eval = evaluar(pesos=vini, datos=datos, label=label)
        if (eval < mejor_eval) {
            # si es mejor actualizamos los valores
            mejor_vini = vini
            mejor_eval = eval
        }
        i = 1+i # incrementamos el indice
    }
    # parámetros a y b del hiperplano del perceptrón y num iteraciones
    c((-mejor_vini[1]/mejor_vini[3]), (-mejor_vini[2]/mejor_vini[3]), i)
}

ej_seis <- function() {
    # obtenemos las etiquetas de los puntos del ejercicio 3
    nombres = etiqueta_puntos(puntos=punt_3)

    for (i in 1:length(funciones_xy)) {
        # dibujamos la recta con los puntos
        dibuja_funcion(puntos=punt_3, funciones_xy[[i]], nombres[[i]], paste("Ejercicio 6.",i+1,sep=""))
        # y calculamos el perceptrón pocket
        perceptron = ajusta_PLA_MOD(datos=punt_3, label=nombres[[i]])
        # representamos el perceptrón obtenido
        abline(a=perceptron[1], b=perceptron[2], col="red",lwd=2)
        print("Pulsa s para ejecutar el siguiente ejercicio...")
        scan(what=character(), n=1)
    }
}

# print("Ejercicio de Ajuste del Algoritmo Perceptrón")

# print("Apartado 2")
# PLA_recta()

# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 3")
# PLA_curva()
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 4")
# PLA_elipse()
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 5")
# ej_cinco()
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 6")
# ej_seis()

#                       Ejercicio sobre regresión lineal                       #

# 1. Abra el fichero ZipDigits.info disponible en la web del curso y lea la
# descripción de la representación numérica de la base de datos de números 
# manuscritos que hay en el fichero ZipDigits.train

# 2. Lea el fichero ZipDigits.train dentro de su código y visualice las imágenes.
# Seleccione sólo las instancias de los números 1 y 5. Guárdelas como matrices de
# tamaño 16x16.
lee_fichero <- function (fichero="zip.train") {
    # leemos el fichero de datos de entrenamiento
    zip <- read.table(fichero, row.names=NULL, quote="\"", comment.char="",
        stringsAsFactors=F)

    # guardamos todas las instancias que sean o bien unos o bien cincos
    unoscincos = zip[zip$V1 == 1 | zip$V1 == 5, 1:257]

    # devolvemos las listas de instancias
    unoscincos
}

representa_numero <- function(matriz) {
    # configuramos las dimensiones para que sean 16x16
    dim(matriz) = c(16,16)
    # representamos la matriz
    image(matriz)
}

pintar_numeros <- function(numeros=lee_fichero()) {
    # representamos un uno
    representa_numero(as.matrix(numeros[[1]][1,]))

    print("Pulsa s para ejecutar la siguiente gráfica...")
    scan(what=character(), n=1)

    # y un cinco
    representa_numero(as.matrix(numeros[[2]][1,]))
}

# 3. Para cada matriz de números calcularemos su valor medio y su grado de simetría
# vertical. Para calcular la simetría calculamos la suma del valor absoluto de las 
# diferencias en cada píxel entre la imagen original y la imagen que obtenemos 
# inviriendo el orden de las columnas. Finalmente cambiamos el signo.
calcula_media <- function(nums) apply(X=nums, FUN=mean, MARGIN=1)
calcula_sim <- function(nums) apply(X=nums, 
    FUN=function(fila) -sum(abs(fila[1:256] - fila[256:1])), MARGIN=1)

valor_medio_simetrico <- function(matrices=lee_fichero()) {
    # guardamos en una variable las instancias con unos
    unos = matrices[[1]]
    # y en otra, las instancias con cincos
    cincos = matrices[[2]]
    # devolvemos una lista con las medias y simetrias de cada uno
    list(list(calcula_media(unos), calcula_sim(unos)), 
        list(calcula_media(cincos), calcula_sim(cincos)))
}

# 4. Representar en los ejes {x = Intensidad Promedio, Y = Simetría} las instancias
# seleccionadas de unos y de cincos.
representa_param <- function(parametros, titulo, color=615, etiquetas) {
    plot(x=parametros[[1]], y=parametros[[2]], pch=19, lwd=2, 
        col=colors()[color+etiquetas], main=titulo, ylab="Simetría", 
        xlab="Intensidad Promedio")
}

ej_cuatro <- function(parametros=valor_medio_simetrico()) {
    # representamos en una gráfica los puntos unos
    representa_param(parametros=parametros[[1]], 
        titulo="Intensidad promedio y simetría de los unos")

    print("Pulsa s para ejecutar la siguiente gráfica...")
    scan(what=character(), n=1)

    # y otra con los cincos
    representa_param(parametros=parametros[[2]], 
        titulo="Intensidad promedio y simetría de los cincos")
}

# 5. Implementar la función sol = Regress_Lin(datos,label) que permita ajustar un
# modelo de regresión lineal (usar SVD). Los datos de entrada se interpretan igual
# que en clasificación.
sol = Regress_Lin <- function(datos, label) {
    # calculamos la pseudoinversa de datos como (X^T·X)^-1 · X^T
    # asumimos que datos*datos^T es una matriz invertible, es decir, cuadrada
    datos <- cbind(1, datos)

    pseudoinversa = solve(t(datos)%*%datos)%*%t(datos)%*%label
    pseudoinversa0 = pseudoinversa[1,1]
    pseudoinversa1 = pseudoinversa[2,1]

    as.vector(t(pseudoinversa))
}

# 6. Ajustar un modelo de regresión lineal a los datos de (Intensidad promedio,
# Simetría) y pintar la solución junto con los datos. Valorar el resultado.
ajuste_reg <- function(parametros, titulo) {
    # obtenemos el ajuste con regresión lineal
    ajuste = Regress_Lin(parametros[[1]], parametros[[2]])
    # representamos los puntos
    representa_param(parametros=parametros, 
        titulo=titulo)
    # y añadimos el ajuste hecho por la regresión
    lines(x=parametros[[1]], y=ajuste[[2]], lwd=2, col=colors()[278])
}

ej_seis_3 <- function (parametros=valor_medio_simetrico()) {
    # calculamos el ajuste con los unos
    ajuste_reg(parametros[[1]],titulo="Ajuste de la intensidad y simetría de los unos")

    print("Pulsa s para ejecutar la siguiente gráfica...")
    scan(what=character(), n=1)

    # y después, para los cincos
    ajuste_reg(parametros[[2]],titulo="Ajuste de la intensidad y simetría de los cincos")
}

# 7. En este ejercicio exploramos cómo funciona regresión lineal en problemas de 
# clasificación. Para ello generamos datos usando el mismo procedimiento que en 
# ejercicios anteriores. Suponemos X = [−10, 10] × [−10, 10] y elegimos muestras 
# aleatorias uniformes dentro de X . La función f en cada caso será una recta 
# aleatoria que corta a X y que asigna etiqueta a cada punto con el valor de su 
# signo. En cada apartado generamos una muestra y le asignamos etiqueta con la 
# función f generada. En cada ejecución generamos una nueva función f.
#       a) Fijar el tamaño de muestra N = 100. Usar regresión lineal para 
#          encontrar g y evaluar Ein, (el porcentaje de puntos incorrectamente 
#          clasificados). Repetir el experimento 1000 veces y promediar los 
#          resultados ¿Qué valor obtiene para Ein?
#       b) Fijar el tamaño de muestra N = 100. Usar regresión lineal para 
#          encontrar g y evaluar Eout. Para ello generar 1000 puntos nuevos y 
#          usarlos para estimar el error fuera de la muestra, Eout (porcentaje 
#          de puntos mal clasificados). De nuevo, ejecutar el experimento 1000 
#          veces y tomar el promedio. ¿Qué valor obtiene de Eout? Valore los 
#          resultados
#       c) Ahora fijamos N = 10, ajustamos regresión lineal y usamos el vector 
#          de pesos encontrado como un vector inicial de pesos para PLA. Ejecutar
#          PLA hasta que converja a un vector de pesos final que separe 
#          completamente la muestra de entrenamiento. Anote el número de 
#          iteraciones y repita el experimento 1.000 veces ¿Cuál es valor 
#          promedio de iteraciones que tarda PLA en converger? (En cada iteración
#          de PLA elija un punto aleatorio del conjunto de mal clasificados). 
#          Valore los resultados
experimento_a <- function(wlin, datos, etiquetas) {
    # calculamos Ein como Ein(w) = 1/N(w^TX^TXw - 2w^TX^Ty + y^Ty)
    ein = (t(wlin)%*%t(datos)%*%datos%*%wlin - 
        2*t(wlin)%*%t(datos)%*%etiquetas + t(etiquetas)%*%etiquetas)
    ein
}

experimento_b <- function(ein, d, N) {
    # calculamos Eout como Eout(g) = Ein(g) + O(d/N)
    eout = ein + (d/N)
    eout
}

experimento_c <- function() {
    # generamos un conjunto de datos de tamaño 10
    datos_c = simula_unif(N=10, dim=2, rango=-10:10)
    # generamos una recta aleatoria dentro de dicho intervalo
    rect_c = simula_recta(-10:10)
    # y etiquetamos los puntos según la recta generada
    etiquetas_c = obten_param_recta(puntos=datos_c, re=rect_c)
    # obtenemos las etiquetas aproximadas por la regresion y el wlim
    aprox_c = Regress_Lin(datos=datos_c, label=etiquetas_c)
    # obtenemos la recta aproximada por ŷ
    recta_aprox_c = calcula_recta(datos_c[1,1], aprox_c[[2]][1], datos_c[2,1], aprox_c[[2]][2])
    # llamamos a PLA con vini (b,a,1)
    pla = ajusta_PLA(datos=datos_c,label=etiquetas_c,
        vini=c(recta_aprox_c[2], recta_aprox_c[1], 1))
    # devolvemos el número de iteraciones que se han necesitado para converger
    pla[3]
}

ej_siete_3 <- function() {
    eins = vector(length = 1000)
    eouts = vector(length = 1000)
    for (i in 1:1000) {
        # generamos 100 puntos aleatorios en el intervalo [-10,10]
        datos = simula_unif(N=100, dim=2, rango=-10:10)
        # generamos una recta aleatoria dentro del intervalo [-10,10]
        rect = simula_recta(-10:10)
        # etiquetamos los puntos según la recta obtenida
        etiquetas = obten_param_recta(puntos=datos, re=rect)
        # obtenemos las etiquetas aproximadas por la regresión
        aprox = Regress_Lin(datos=datos, label=etiquetas)
        eins[i] = experimento_a(aprox[[1]], datos, etiquetas)
        eouts[i] = experimento_b(eins[i], ncol(datos), nrow(datos))
    }
    cat("Media de Ein = ", mean(eins),"\n")
    cat("Media de Eout = ",mean(eouts),"\n")
    # hacemos el experimento C
    iter = vector(length=1000)
    for (i in 1:1000) {
        iter[i] = experimento_c()
    }
    cat("Media de iteraciones para converger = ", mean(iter),"\n")
}

# 8. En este ejercicio exploramos el uso de transformaciones no lineales. 
# Consideremos la función objetivo f(x1,x2) = sign(x1^2 + x2^2 - 25). Generar una
# muestra de entrenamiento de N = 1000 puntos a partir de X = [-10,10] x [-10,10]
# muestreando cada punto x e X uniformemente. Generar las salidas usando el signo
# de la función en los puntos muestreados. Generar ruido sobre las etiquetas cambiando
# el signo de las salidas a un 10% del conjunto aleatorio generado.
#       * Ajustar regresión lineal para estimar los pesos w. Ejecutar el experimento
#         1000 veces y calcular el valor promedio del error de entrenamiento Ein.
#         Valorar el resultado.
#       * Ahora consideremos N = 1000 datos de entrenamiento y el siguiente vector
#         de variables: (1,x1,x2,x1x2,x1^2,x2^2). Ajustar de nuevo regresión 
#         lineal y calcular el nuevo vector de pesos ŵ. Mostrar el resultado.
#       * Repetir el experimento anterior 1.000 veces calculando en cada ocasión
#         el error fuera de la muestra. Para ello generar en cada ejecución 1.000
#         puntos nuevo y valorar sobre la función ajustada. Promediar los valores
#         obtenidos Â. ¿Qué valor obtiene? Valorar el resultado.
ej_ocho_3 <- function() {
    # procedemos de forma idéntica al ejercicio anterior
    eins = vector(length = 1000)

    for (i in 1:1000) {
        # generamos los datos de entrenamiento
        X = simula_unif(N=1000, dim=2, rango=-10:10)
        # declaramos la función
        funcion_ocho <- function(vec) sign(vec[1]*vec[1] + vec[2]*vec[2] - 25)
        # etiquetamos los puntos con la función
        etiquetas = calcula_nombres(funcion=funcion_ocho, puntos=X)
        # metemos ruido en las etiquetas generadas
        etiquetas = mete_ruido(nombres=etiquetas)
        eins[i] = experimento_a(Regress_Lin(datos=X,label=etiquetas)[[1]], X, etiquetas)
    }
    cat("Media de Ein = ",mean(eins),"\n")

    # creamos la matriz de variables con 1,x1,x2,x1x2,x1*x1,x2*x2:
    vector_variables = t(apply(X=X, MARGIN=1,
        FUN=function(p) c(1,p[1],p[2],p[1]*p[2],p[1]*p[1],p[2]*p[2])))
    # Pintamos la función
    dibuja_funcion(puntos=X, function(x,y) sign(x*x + y*y - 25), etiquetas, "Ejercicio 8")
    # le añadimos el w calculado
    lines(x=X[,1], y=Regress_Lin(datos=vector_variables,label=etiquetas)[[2]],col="red",lwd=2)

    # Repetimos lo anterior 1000 veces
    eouts = vector(length = 1000)
    for (i in 1:1000) {
        # generamos los datos de entrenamiento
        X = simula_unif(N=1000, dim=2, rango=-10:10)
        # declaramos la función
        funcion_ocho <- function(vec) sign(vec[1]*vec[1] + vec[2]*vec[2] - 25)
        # etiquetamos los puntos con la función
        etiquetas = calcula_nombres(funcion=funcion_ocho, puntos=X)
        # metemos ruido en las etiquetas generadas
        etiquetas = mete_ruido(nombres=etiquetas)
        eins[i] = experimento_a(Regress_Lin(datos=vector_variables,
            label=etiquetas)[[1]], vector_variables, etiquetas)
        eouts[i] = experimento_b(eins[i], ncol(vector_variables), 
            nrow(vector_variables))
    }
    cat("Media de Eouts = ",mean(eouts),"\n")
}

# print("Ejercicio sobre regresión linal")

# print("Apartado 2")
# pintar_numeros()

# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 3")
# print(valor_medio_simetrico())
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 4")
# ej_cuatro()
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 6")
# ej_seis_3()

# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 7")
# ej_siete_3()

# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)

# print("Apartado 8")
# ej_ocho_3()
