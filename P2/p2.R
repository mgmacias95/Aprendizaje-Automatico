################################################################################
#                     PRÁCTICA 2 APRENDIZAJE AUTOMÁTICO
#                            Marta Gómez Macías
#                                 Grupo 1
#                           1 de Abril de 2016
################################################################################

#                             Modelos lineales                                 #
# 1. Implementar el algoritmo de gradiente descendiente. 
#       a) Considerar la función no lineal de error E(u,v) = (ue^v - 2ve^(-u))^2.
#          Usar gradiente descendente y minimizar esta función de error, comenzando
#          desde el punto (u,v)=(1,1) y usando una tasa de aprendizaje nu=0.1
#               1) Calcular analíticamente y mostrar la expresión del gradiente
#                  de la función E(u,v)
#               2) ¿Cuántas iteraciones tarda el algoritmo en obtener por primera
#                  vez un valor de E(u,v) inferior a 10^(-14). (Usar flotantes de
#                  64 bits).
#               3) ¿Qué valores de (u,v) obtuvo en el apartado anterior cuando
#                  alcanzó el error de 10^(-14).
#       b) Considerar ahora la función f(x,y) = x^2 + 2y^2 + 2sin(2 pi x)sin(2 pi y)
#               1) Usar gradiente descendente para minimizar esta función. Usar
#                  como valores iniciales x0 = 1, y0 = 1, la tasa de aprendizaje
#                  nu = 0.01 y un máximo de 50 iteraciones. Generar un gráfico de
#                  cómo desciende el valor de la función con las iteraciones. 
#                  Repetir el experimento pero usando n = 0.1, comentar las 
#                  diferencias.
#               2) Obtener el valor mínimo y los valores de las variables que lo
#                  alcanzan cuando el punto de inicio se fija: (0.1,0.1),(1.1),
#                  (-0.5,-0.5),(-1,-1). Generar una tabla con los valores obtenidos
#                  ¿Cuál sería su conclusión sobre la verdadera dificultad de 
#                  encontrar el mínimo global de una función arbitraria?

source("../P1/p1.R")
library("Deriv")
library(ggplot2)
library("orthopolynom") # para polinomios de Legendre

# Version con las derivadas hechas por mí
fau <- function(u,v) 2*(u*exp(v) - 2*v*exp(-u)) * (exp(v) + 2*v*exp(-u))
fav <- function(u,v) 2*(u*exp(v) - 2*v*exp(-u)) * (u*exp(v) - 2*exp(-u))

funciones <- c(function(x,y) (x*exp(y) - 2*y*exp(-x))^2,
    function(x,y) x^2 + 2*y^2 + 2*sin(6.28*x)*sin(6.28*y))

gradient_descent <- function(eta=0.1, precision=10^(-14), init=c(1,1),
    fx = fau, fy=fav, fu = funciones[[1]], max_iter=50) {
    x_old = 0
    y_old = 0
    x_new = init[1]     
    y_new = init[2]
    iter = 0            
    while(abs(fx(x_new,y_new)) > precision & iter < max_iter
        & abs(x_new - x_old) > precision) {
        iter = iter + 1
        x_old = x_new
        y_old = y_new
        x_new = x_old - eta * fx(x_old, y_old)
        y_new = y_old - eta * fy(x_old, y_old)
    }
    c(fu(x_new,y_new),x_new,y_new,iter)
}

# Version con las derivadas hechas por Deriv
gradient_descent_deriv <- function(eta=0.1, precision=10^(-14), init=c(1,1), 
    fu = funciones[[1]], max_iter=50) {
    
    df = Deriv(f=fu,x=c('x','y'))
    x_old = 0
    y_old = 0
    x_new = init[1]
    y_new = init[2]
    iter = 0
    while(norm(as.matrix(df(x_new,y_new)), type="F") > precision & iter < max_iter 
            & abs(fu(x_new, y_new) - fu(x_old, y_old)) > precision) {
        iter = iter + 1
        x_old = x_new
        y_old = y_new
        derivada = df(x_old, y_old)
        x_new = x_old - eta * derivada[1]
        y_new = y_old - eta * derivada[2]
    }
    c(fu(x_new,y_new),x_new,y_new,iter)
}

# Versión con las derivadas hechas por Deriv y que hace un gráfico de cómo va
# evolucionando el gradiente
dibuja_funcion_sin_puntos <- function(funcion, titulo,intervalo=c(-3,3)) {
    x <- y <- seq(intervalo[1],intervalo[2],length=100) 
    z <- outer(x,y,funcion)             # calculamos los puntos de la funcion
    contour (                           # la representamos
        x=x, y=x, z=z,
        levels=1, las=1, drawlabels=FALSE, main=titulo, col="red"
    )
}

dibuja_funcion_puntos <- function (puntos = puntos_funcion, func=funciones[[2]]) {
    p = matrix(puntos[!is.na(puntos)], ncol=3)
    r = apply(X=p, MARGIN=1, FUN=function(vec) func(vec[1],vec[2]))
    datos = cbind(p[,3], r)
    plot(datos, type="l", lwd=2, col="blue")
    abline(a=0,b=0, lwd=2, col="red")
}

gradient_descent_deriv_grafica <- function(eta=0.01, precision=10^(-14), init=c(1,1),
    fu = funciones[[2]], max_iter=50, intervalo_g=c(-2,2)) {
    
    df = Deriv(f=fu,x=c('x','y'))
    x_old = 0
    y_old = 0
    x_new = init[1]
    y_new = init[2]
    iter = 0
    puntos_funcion = matrix(ncol=3, nrow=max_iter) # matriz vacia para guardar puntos 
    dibuja_funcion_sin_puntos(funcion=fu, titulo=eta, intervalo=intervalo_g)

    while(norm(as.matrix(df(x_new,y_new)), type="F") > precision & iter < max_iter 
            & abs(fu(x_new, y_new) - fu(x_old, y_old)) > precision) {
        points(x=x_new, y=y_new, col="blue", lwd=2, pch=19)
        puntos_funcion[iter+1,] = c(x_new, y_new, iter)
        iter = iter + 1
        x_old = x_new
        y_old = y_new
        derivada = df(x_old, y_old)
        x_new = x_old - eta * derivada[1]
        y_new = y_old - eta * derivada[2]
    }
    print("Pulsa s para ejecutar la siguiente gráfica...")
    scan(what=character(), n=1)
    dibuja_funcion_puntos(puntos_funcion, fu)
    c(fu(x_new,y_new),x_new,y_new,iter)
}

# 2. En este ejercicio comparamos la eficiencia de la técnica de optimización de
# "coordenada descendente" usando la misma función del ejercicio 1.1a. En cada 
# iteración, tenemos dos pasos a lo largo de dos coordenadas. En el Paso-1 nos
# movemos a lo largo de la coordenada u para reducir el error (suponer que se 
# verifica una aproximación de primer orden como en gradiente descendente), y el
# Paso-2 es para reevaluar y movernos a lo largo de la coordenada v para reducir
# el error (hacer la misma hipótesis que en el Paso-1). Usar una tasa de aprendizaje
# n = 0.1.
#       a) ¿Qué valor de la función E(u,v) se obtiene después de 15 iteraciones 
#          completas?
#       b) Establezca una comparación entre esta técnica y la técnica de gradiente
#          descendente.
coordenada_descendente <- function(max_iter=15,fu=funciones[[1]], eta=0.1, 
    init=c(1,1), precision=10^(-14)) {
    
    df = Deriv(f=fu,x=c('x','y'))
    x_new = init[1]
    y_new = init[2]
    x_old = 0
    y_old = 0
    iter = 0
    while(iter < max_iter & norm(as.matrix(df(x_new,y_new)), type="F") > precision) {
        iter = iter + 1
        # Paso 1: nos movemos a lo largo de la coordenada u
        x_old = x_new
        derivada = df(x_old, y_old)
        x_new = x_old - eta * derivada[1]
        # Paso 2: reevaluamos y nos movemos a lo largo de la coordenada v
        derivada = df(x_new, y_old)
        y_new = y_old - eta * derivada[2]
    }
    c(fu(x_new,y_new),x_new,y_new, iter)
}

# 3. Implementar el algoritmo de minimización de Newton y aplicarlo a la función
# f(x,y) dada en el ejercicio 1.1b. Desarrolle los mismos experimentos usando los
# mismos puntos de inicio.
#       * Generar un gráfico de cómo desciende el valor de la función con las 
#         iteraciones.
#       * Extraer conclusiones sobre las conductas de los algoritmos comparando
#         la curva de decrecimiento de la función calculada en el apartado
#         anterior y la correspondiente obtenida con gradiente descendente.
newton <- function(fu=funciones[[2]], init=c(1,1), precision=10^(-14), max_iter=50,
    intervalo_g=c(-2,2)) {
    
    df = Deriv(f=fu,x=c('x','y'), nderiv=1:2)
    x_old = 0
    y_old = 0
    x_new = init[1]
    y_new = init[2]
    iter = 0

    puntos_funcion = matrix(ncol=3, nrow=max_iter)
    dibuja_funcion_sin_puntos(funcion=fu, titulo=paste("x=",init[1]," y=",init[2]), 
        intervalo = intervalo_g)

    while(iter < max_iter & abs(fu(x_old, y_old) - fu(x_new, y_new)) > precision &
            norm(as.matrix(df(x_new, y_new)$`1`), type="F") > precision) {
        points(x=x_new, y=y_new, col="blue", lwd=2, pch=19)
        puntos_funcion[iter+1,] = c(x_new, y_new, iter)
        iter = iter + 1
        x_old = x_new
        y_old = y_new
        derivada = df(x_old, y_old)
        d2inversa = solve(matrix(derivada$`2`, ncol=2))
        # formula: http://web.stanford.edu/class/msande311/lecture13.pdf pag 6
        x_new = x_old - d2inversa[1,] %*% derivada$`1`
        y_new = y_old - d2inversa[2,] %*% derivada$`1`
    }
    print("Pulsa s para ejecutar la siguiente gráfica...")
    scan(what=character(), n=1)
    dibuja_funcion_puntos(puntos_funcion, fu)
    c(fu(x_new,y_new),x_new,y_new,iter)
}

# 4. En este ejercicio crearemos nuestra propia función objetivo f (probabilidad 
# en este caso) y nuestro conjunto de datos D para ver cómo funciona 
# regresión logística. Supondremos por simplicidad que f es una probabilidad con 
# valores 0/1 y por tanto que y es una función determinista de x.
# Consideremos d = 2 para que los datos sean visualizables, y sea X = [-1,1] x [-1,1] 
# con probabilidad uniforme de elegir cada x e X. Elegir una línea en el plano 
# como la frontera entre f(x) = 0 (donde y toma valores -1), para ello seleccionar 
# dos puntos aleatorios del plano y calcular la línea que pasa por ambos.

# Seleccionar N = 100 puntos aleatorios x_n de X y evaluar las respuestas de 
# todos ellos y_n respecto de la frontera elegida.

# a) Implementar Regresión Logística (RL) con Gradiente Descendente Estocástico 
#    (SGD) bajo las siguientes condiciones:
#     * Inicializar el vector de pesos con valores 0.
#     * Parar el algoritmo cuando ||w^{(t-1)} - w^{(t)}|| < 0,01, donde w^{(t)} 
#       denota el vector de pesos al final de la época t. Una época es un pase 
#       completo a través de los N datos.
#     * Aplicar una permutación aleatoria de 1,2,...,N a los datos antes de 
#       usarlos en cada época del algoritmo.
#     * Usar una tasa de aprendizaje de eta = 0,01.
# b) Usar la muestra de datos etiquetada para encontrar g y estimar E_out (el 
#    error de entropía cruzada) usando para ello un número suficientemente grade 
#    de nuevas muestras.
# c) Repetir el experimento 100 veces con diferentes funciones frontera y calcule el promedio.
#     1) ¿Cuál es el valor de E_out para N=100?
#     2) ¿Cuántas épocas tarda en promedio RL en converger para N=100, usando 
#        todas las condiciones anteriormente especificadas?
cross_entropy_error <- function(wlin, datos, recta) {
    etiquetas = obten_param_recta(puntos=datos, re=recta)
    # datos = cbind(rep(1, nrow(datos)), datos)
    aux = apply(X=datos, FUN = function(d) d%*%wlin, MARGIN=1)
    error = mapply(FUN = function(d, l) log (1 + exp(-l*d)), d = aux, 
        l = etiquetas)

    mean(error)
}

logistic_regression_stochastic_gradient_descent <- function(eta = 0.01, max_iter=150, 
    datos = simula_unif(dim=2, N=100, rango=-1:1), etiquetas) {
    w_new = rep(0,ncol(datos))
    w_old = w_new+1 # inicializamos el vector de pesos
    # añadimos a la matriz datos una tercera columna para poder hacer el producto vectorial
    # datos = cbind(rep(1,nrow(datos)), datos)
    i = 0

    mod <- function(w) {
        raiz = sapply(X=w, FUN=function(x) x*x)
        sqrt(sum(raiz))
    }

    # parada: modulo de la diferencia de los dos vectores
    while (mod(w_old - w_new) >= 0.01 & i < max_iter) {
        i = i + 1
        # hacemos una permutación aleatoria de los datos
        w_old = w_new
        datos = datos[sample.int(nrow(datos)),]
        for (j in 1:nrow(datos)) {
            # calculamos el gradiente de error para cada punto con la fórmula
            # de la página 98 del libro de teoría
            error = (-etiquetas[j]*datos[j,])/(1 + exp(etiquetas[j]*(w_new%*%datos[j,])))
            # actualizamos el peso usando la probabilidad de error de ese punto
            w_new = w_new - eta/nrow(datos) * error
        }
    }
    w_new
}

# 5. Considerar el conjunto de datos de los dígitos manuscritos y seleccionar las
# muestras de los dígitos 1 y 5. Usar los ficheros de entrenamiento (training) y
# test que se proporcionan. Extraer las características de intensidad promedio y
# simetría en la manera que se indicó en el ejercicio 3 del trabajo 1.
# Plantear un problema de clasificación binaria que considere el conjunto de 
# entrenamiento como datos de entrada para aprender la función g. Usando el modelo
# de Regresión Lineal para clasificación seguido por PLA-Pocket como mejora. 
# Responder a las siguientes cuestiones:
#       a) Generar gráficos separados (en color) de los datos de entrenamiento y
#          test junto con la función estimada.
#       b) Calcular Ein y Etest (error sobre los datos de test)
#       c) Obtener cotas sobre el verdadero valor de Eout. Pueden calcularse dos
#          cotas, una basada en Ein y otra basada en Etest. Usar una tolerancia
#          delta = 0.05. ¿Qué cota es mejor?
#       d) Repetir los puntos anteriores pero usando una transformación polinómica
#          de tercer orden (phi_3 (x) en las transparencias de teoría) (opcional)
#       e) Si tuviera que usar los resultados para dárselos a un potencial cliente,
#          ¿usaría la transformación polinómica? Explicar la decisión. (opcional)
ajusta_fichero <- function(fichero="zip.train") {
    # obtenemos los datos
    fichero <- lee_fichero(fichero)
    # calculamos las etiquetas en función de si el número es 1 o no
    etiquetas <- apply(X=fichero, MARGIN=1, 
        FUN=function(fila) {
            s = 0
            if (fila[1] == 1) {
                s = 1
            } else {
                s = -1
            }
            s
        })
    datos <- fichero[,2:257]
    # calculamos la media y la simetria
    sim <- calcula_sim(datos)
    med <- calcula_media(datos)

    list(med,sim,etiquetas)
}

regresion_pla <- function(parametros) {
    ajuste = Regress_Lin(parametros[[2]], parametros[[1]])
    # con el pocket tarda demasiado en converger
    pla = ajusta_PLA(datos=matrix(c(parametros[[1]],parametros[[2]]), ncol=2),
        label=parametros[[3]],vini=c(ajuste[2], ajuste[1], 1), max_iter=50)
    list(ajuste, pla)
}

cota_eout <- function(N, dvc, delta=0.05, e) {
    eout = e + sqrt(8/N * log((4 * (2*N)^dvc + 1)/delta))
    eout
} 

ej_5 <- function() {
    # ajustamos los datos de entrenamiento
    train <- ajusta_fichero()
    trainpla <- regresion_pla(train)
    phi_3 <- phi3(train)

    # ajustamos los datos de test
    test <- ajusta_fichero(fichero="zip.test")

    # representamos la regresión
    representa_param(parametros=list(train[[1]],train[[2]]),titulo="Training", 
        etiquetas=train[[3]])
    print(trainpla)
    abline(a=trainpla[[2]][1], b=trainpla[[2]][2], col="red", lwd=2)
    print("Pulsa s para ejecutar la siguiente gráfica...")
    scan(what=character(), n=1)
    representa_param(parametros=list(test[[1]],test[[2]]),titulo="Test", 
        etiquetas=test[[3]])
    abline(a=trainpla[[2]][1], b=trainpla[[2]][2], col="red", lwd=2)
    
    # calculamos E_in y E_test:
    ein = apply(X=cbind(train[[1]], train[[2]], train[[3]]), MARGIN=1, 
        FUN=function(vec) experimento_a(wlin=trainpla[[1]], datos=cbind(vec[1], 
            vec[2]), etiquetas=vec[3]))
    etest = apply(X=cbind(test[[1]], test[[2]], test[[3]]), MARGIN=1, 
        FUN=function(vec) experimento_a(wlin=trainpla[[1]], datos=cbind(vec[1], 
            vec[2]), etiquetas=vec[3]))
    # etest = experimento_b(mean(ein), 1, length(train[[1]]))
    cat("Ein = ", mean(ein),"\n")
    cat("Etest = ", mean(etest),"\n")

    # y por último, la cota de eout. Como estamos en un modelo lineal, dvc = 3
    eout_in = cota_eout(N=length(train[[1]]), dvc=3, e=mean(ein))
    eout_test = cota_eout(N=length(train[[1]]), dvc=3, e=mean(etest))

    cat("Eout (basado en Ein) = ", eout_in,"\n")
    cat("Etest (basado en Etest) = ", eout_test, "\n")
}

# print("Modelos lineales")
# print("EJERCICIO 1")
# print("Apartado a")
# print("Gradiente descendente sin usar Deriv")
# print(gradient_descent())
# print("Gradiente descendente usando Deriv")
# print(gradient_descent_deriv())
# print("Apartado b1")
# print(gradient_descent_deriv_grafica())
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)
# print(gradient_descent_deriv_grafica(eta=0.1))
# print("Apartado b2")
# print("Punto de inicio x=0,1 e y=0,1")
# print(gradient_descent_deriv(eta=0.01, init=c(0.1,0.1), fu=funciones[[2]]))
# print("Punto de inicio x=1 e y=1")
# print(gradient_descent_deriv(eta=0.01, init=c(1,1), fu=funciones[[2]]))
# print("Punto de inicio x=-0,5 e y=-0,5")
# print(gradient_descent_deriv(eta=0.01, init=c(-0.5,-0.5), fu=funciones[[2]]))
# print("Punto de inicio x=-1 e y=-1")
# print(gradient_descent_deriv(eta=0.01, init=c(-1,-1), fu=funciones[[2]]))
# print("EJERCICIO 2")
# print(coordenada_descendente())
# print("EJERCICIO 3")
# print("Punto de inicio x=0.1 e y=0.1")
# print(newton(init=c(0.1,0.1)))
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)
# print("Punto de inicio x=1 e y=1")
# print(newton(init=c(1,1)))
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)
# print("Punto de inicio x=-0.5 e y=-0.5")
# print(newton(init=c(-0.5,-0.5)))
# print("Pulsa s para ejecutar el siguiente ejercicio...")
# scan(what=character(), n=1)
# print("Punto de inicio x=-1 e y=-1")
# print(newton(init=c(-1,-1)))
# print("EJERCICIO 4")
# r = simula_recta()
# sgd = logistic_regression_stochastic_gradient_descent(etiquetas=obten_param_recta(re=r))
# print(sgd)
# cat("Cross entropy error: ", cross_entropy_error(wlin=sgd, 
#     datos = simula_unif(dim=2, N=100, rango=-1:1), recta=r), "\n")
# print("EJERCICIO 5")
# ej_5()

#                                Sobreajuste                                   #
# 1. Vamos a construir un entorno que nos permita experimentar con los problemas 
# de sobreajuste. Consideremos el espacio de entrada X=[-1,1] con una densidad 
# de probabilidad uniforme, P(x) = 1/2. Consideramos dos modelos H2 y H10 
# representando el conjunto de todos los polinomios de grado 2 y grado 10 
# respectivamente. La función objetivo es un polinomio de grado Qf que escribimos
# como f(x) = sum_q=0 ^ Qf aqLq(x), donde Lq(x) son los polinomios de Legendre.
# El conjunto de datos D = {(x1,y1),...,(xN,yN)} donde yn = f(xn) + sigma epsilon n
# y las {epsilon n} son variables aleatorias i.i.d. N(0,1) y sigma^2 la varianza
# del ruido.

# Comenzamos realizando un experimento donde suponemos que los valores Qf, N, sigma
# están especificados, para ello:
#       * Generamos los coeficientes aq a partir de muestras de una distribución
#         N(0,1) y escalamos dichos coeficientes de manera que Ea,x[f^2] = 1 (Ayuda:
#         dividir los coeficientes por sqrt(sum_q=0^Qf 1/2q+1))
#       * Generamos un conjunto de datos x1,...xN muestreando de forma independiente
#         P(x) y los valores yn = f(xn) + sigma epsilon n

# Sean g2 y g10 los mejores ajustes a los datos usando H2 y H10 respectivamente,
# y sean Eout(g2) y Eout(g10) sus respectivos errores fuera de la muestra
#       a) Calcular g2 y g10
#       b) ¿Por qué normalizamos f? (Ayuda: interpretar el significado de sigma)
#       c) [Opcional] ¿Cómo podemos obtener Eout analíticamente para una g10 dada?
pintar_ggplot <- function(datos, x, yn, g2, g10) {
    graph <- (ggplot() + geom_point(data=as.data.frame(datos), aes(x=x, y=yn)) 
        + geom_line(aes(x=x, y=predict(g2), color="g2")) 
        + geom_line(aes(x=x, y=predict(g10), color="g10")))
}

ej_2_1 <- function(intervalo=-1:1, prob=0.5, N=100, sigma=1, Qf=10) {
    # generamos los coeficientes con la distribución normal N(0,1)
    aq = as.vector(simula_gaus(N=Qf+1, dim=1, sigma=c(1)))
    d = sqrt(sum(sapply(X=0:Qf, FUN=function(q) 1/(2*q+1))))
    # escalamos los coeficientes generados
    aq = aq/d
    # generamos la función objetivo, como la suma de los polinomios de legendre 
    # desde grado 0 hasta 20
    legendre = legendre.polynomials(n=Qf, normalized=T) # generamos los 20 primeros
    f = legendre[[1]]*aq[1]
    # sumamos los polinomios de legendre. NO se puede hacer con sum
    for (i in 2:length(legendre)) 
        f = f + legendre[[i]] * aq[i]
    # generamos los datos
    x = as.vector(simula_unif(N=N, dim=1, rango=intervalo))
    # generamos epsilon_n
    epsilon_n = as.vector(simula_gaus(N=N, dim=1, sigma=c(1)))
    # calculamos la funcion de etiquetado como f_e = f(x) + sigma * epsilon_n
    # para ello: 1. calculamos todos los f(x)
    fx = sapply(X=x, FUN=as.function(f))
    # 2. le sumamos a los fx calculados, sigma*epsilon
    yn = mapply(FUN=function(valor, en) valor + sigma*en, valor=fx, en=epsilon_n)
    # generamos la matriz de datos
    datos = cbind(x, yn)
    # y los datos que queremos ajustar
    datos_predecir = poly(x, degree=Qf)
    # hayamos los modelos para cada h
    g2 <- lm(yn ~ poly(x, degree=2),data=datos_predecir)
    g10 <- lm(yn ~ poly(x, degree=10),data=datos_predecir)
    # y los representamos en una gráfica
    list(datos, x, yn, g2, g10)
}

# 2. Siguiendo con el punto anterior, usando la combinación de parámetros Qf=20,
# N = 50, sigma=1 ejecutar un número de experimentos (>100) calculando en cada caso
# Eout(g2) y Eout(g10). Promediar todos los valores de error obtenidos para cada
# conjunto de hipótesis, es decir
#       Eout(H2) = promedio sobre experimentos (Eout(g2))
#       Eout(H10) = promedio sobre experimentos (Eout(g10))
# Definimos una medida de sobreajuste como Eout(H10) - Eout(H2).
#       a) Argumentar por qué la medida dada puede medir el sobreajuste
#       b) [Opcional] Usando la combinación de valores de los valores Qf e {1,2,...,100}
#          N e {20,25,...,100}, sigma e {0,0.05,0.1,...,2} se obtiene una gráfica
#          como la que aparece en la figura 4.3 del libro "Learning from data",
#          capítulo 4. Interpreta la gráfica respecto a las condiciones en las 
#          que se da el sobreajuste. (Nota: no es necesario la implementación).
ein <- function(w, x, y) {
    # calculamos Ein(w) como se indica en la página 91:
    # Ein(w) = 1/N sum_n=1^N (w^T z_n - y_n)^2
    errores = apply(X=x, MARGIN=1, FUN=function(dat) t(w)%*%dat)
    ein = mapply(FUN=function(e,yn) log(1+exp(-yn*e)), e=errores, yn=y)
    mean(ein)
}

calcula_errores <- function(x, yn, g, grado, N) {
    datos = cbind(1, poly(x, degree=grado))
    ein = ein(w=g, x=datos, y=yn)
    eout = cota_eout(e=ein, dvc=grado+1, N=N)
    c(eout,ein)
}

experimento <- function() {
    pesos = ej_2_1(Qf=15, N=50, sigma=1)
    errores_g2 = calcula_errores(x=pesos[[2]], yn=pesos[[3]], grado=2, 
        N=50, g=as.vector(pesos[[4]]$'coefficients'))
    errores_g10 = calcula_errores(x=pesos[[2]], yn=pesos[[3]], grado=10,
        N=50, g=as.vector(pesos[[5]]$'coefficients'))
    list(errores_g2,errores_g10)
}

ej_2_2 <- function() {
    eg2 = 0
    eg10 = 0
    eing2 = 0
    eing10 = 0
    for (i in 1:100) {
        errores = experimento()
        eg2 = eg2 + errores[[1]][1]
        eg10 = eg10 + errores[[2]][1]
        eing2 = eing2 + errores[[1]][2]
        eing10 = eing10 + errores[[2]][2]
    }
    eg2 = eg2/100
    eg10 = eg10/100
    eing2 = eing2/100
    eing10 = eing10/100
    cat("Eout(g2) = ", eg2, "\n")
    cat("Eout(g10) = ", eg10, "\n")
    cat("Ein(g2) = ",eing2,"\n")
    cat("Ein(g10) = ",eing10,"\n")
    eg10 - eg2
}

# print("Sobreajuste")
# print("EJERCICIO 1")
# l <- ej_2_1()
# cat("g2 = ",as.vector(l[[4]]$'coefficients'),"\n")
# cat("g10 = ",as.vector(l[[5]])$'coefficients',"\n")
# print(pintar_ggplot(datos=l[[1]], x=l[[2]], yn=l[[3]], g2=l[[4]], g10=l[[5]]))
# print("EJERCICIO 2")
# print(ej_2_2())

#                     Regularización y Selección de Modelos                    #
# 1. Para d=3 (dimensión) generar un conjunto de N datos aleatorios {x_n,y_n} de
# la siguiente forma. Para cada punto x_n generamos sus coordenadas muestreando de
# forma independiente una N(0,1). De forma similar generamos un vector de pesos
# de (d+1) dimensiones wf, y el conjunto de valores yn = w_f^T*x_n+sigma*epsilon,
# donde epsilon es un ruido que sigue también una N(0,1) y sigma^2 es la varianza
# del ruido; fijar sigma = 0.5
# Usar regresión lineal con regularización "weight decay" para estimar w_f con w_reg.
# Fijar el parámetro de regularización a 0,05/N.
#       a) Para N e {d+15,d+25,...,d+115} calcular los errores e1,...,eN de 
#          validación cruzada y Ecv.
#       b) Repetir el experimento 1000 veces, anotando el promedio y la varianza
#          de e1, e2 y Ecv en todos los experimentos.
#       c) ¿Cuál debería de ser la relación entre el promedio de los valores e1 y
#          el de los valores de Ecv? ¿y el de los valores de e2? Argumentar la
#          respuesta en base a los resultados de los experimentos.
#       d) ¿Qué es lo que contribuye a la varianza de los errores de e1?
#       e) Si los errores de validación cruzada fueran verdaderamente independientes,
#          ¿cuál sería la relación entre la varianza de los valores de e1 y la
#          varianza de los de Ecv?
#       f) Una medida del número efectivo de muestras nuevas usadas en el cálculo
#          de Ecv es el coeficiente entre la varianza de e1 y la varianza de Ecv.
#          Explicar por qué, y dibujar, respecto de N, el número efectivo de nuevos
#          ejemplos (Neff) como un porcentaje de N. NOTA: Debería encontrarse que
#          Neff está cercano a N.
#       g) Si se incrementa la cantidad de regularización, ¿debería Neff subir o 
#          bajar? Argumentar la respuesta. Ejecutar el mismo experimento con 
#          lambda = 2.5/N y comparar los resultados del punto anterior para
#          verificar la conjetura.
weight_decay <- function(datos, y, lambda){
    as.vector(solve(t(datos)%*%datos+lambda*diag(ncol(datos)))%*%t(datos)%*%y)
}


cross_validation_n <- function(d=2, sigma=0.5, lambda=0.05) {
    # generamos los tamaños de N en los que haremos el experimento
    tams = seq(from=15, to=115, by=10)
    lista_ecv = vector() # vector para guardar todos los errores de validación cruzada
    e1 = vector() # vector para guardar todos los e1 obtenidos
    e2 = vector() # vector para guardar todos los e2 obtenidos
    wf = as.vector(simula_gaus(N=1, dim=d+1, sigma=c(1))) # los pesos
    for (i in tams) {
        n = i+d # generamos el tamaño de los datos en esta iteración
        lambda = lambda/n
        x = simula_gaus(N=n, dim=d, sigma=c(1)) # generamos los datos
        epsilon = as.vector(simula_gaus(N=n, dim=1, sigma=c(1))) # el ruido
        # y por último, las etiquetas
        x_aux = cbind(rep(1,n), x)
        fx = apply(X=x_aux, MARGIN=1, FUN=function(fila) wf%*%fila)
        yn = mapply(FUN=function(f, e) f + e*sigma, f=fx, e=epsilon)
        # definimos el error en regresión.
        squared_error <- function(w, x, y) {
            hi = w%*%x
            (hi-y)*(hi-y)
        }
        # y calculamos el error para cada punto
        e = sapply(X=1:n, FUN=function(i) squared_error(w=weight_decay(datos=x_aux[-i,], 
            y=yn[-i], lambda=lambda), x=x_aux[i,], y=yn[i]))
        lista_ecv = append(x=lista_ecv, values=mean(e))
        e1 = append(x=e1, values=e[1])
        e2 = append(x=e2, values=e[2])
    }
    list(lista_ecv, e1, e2)
}

experimento_3_1_b <- function(veces=1000) {
    resultados = matrix(ncol=6, nrow=veces)

    for (i in 1:veces) {
        l = cross_validation_n()
        resultados[i,] = c(mean(l[[1]]), var(l[[1]]), mean(l[[2]]), var(l[[2]]),
            mean(l[[3]]), var(l[[3]]))
    }

    colnames(resultados)=c("Media Ecv", "Var Ecv", "Media e1", "Var e1", "Media e2", "Var e2")
    resultados
}

nuevos_ejemplos <- function(veces=1000, d=2, lambda=0.05) {
    # ejecutamos el apartado a varias veces para obtener listas completas de Ecv y e1
    # para ello: creamos una matriz para guardar los distintos valores de Ecv y e1
    # para cada N usado.
    valores_ecv = matrix(ncol=11, nrow=veces)
    valores_e1 = matrix(ncol=11, nrow=veces)
    for (i in 1:veces) {
        listas = cross_validation_n(lambda=lambda)
        valores_ecv[i,] = as.vector(listas[[1]])
        valores_e1[i,] = as.vector(listas[[2]])
    }
    # una vez obtenidas las listas de valores de e1 y Ecv calculamos la varianza
    var_ecv = apply(X=valores_ecv, MARGIN=2, FUN=function(col) var(col))
    var_e1 = apply(X=valores_e1, MARGIN=2, FUN=function(col) var(col))
    # hacemos el cociente de los elementos de ambos vectores
    var_cociente = var_e1/var_ecv
    # representamos en una gráfica en el eje X los N y en el y, los cocientes:
    tams = seq(from=15+d, to=115+d, by=10)
    plot(x=tams, y=var_cociente, lwd=2, pch=19, ylab="Neff", xlab="N")
    (var_cociente/tams)*100
}

# print("Regularización y selección de modelos")
# print("Ejercicio 1")
# print("Apartado a")
# print(cross_validation_n()[[1]])
# print("Apartado b")
print(experimento_3_1_b(veces=10))
# print("Apartado f")
# print(nuevos_ejemplos())
# print("Apartado g")
# print(nuevos_ejemplos(lambda=25))
